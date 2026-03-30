box::use(
  DBI[dbExecute, dbGetQuery],
  R6[R6Class],
  RSQLite[SQLite],
  checkmate[qtest],
  config,
  data.table[setDT],
  glue[glue_sql],
  logger[log_debug, log_error, log_info, log_trace, log_warn],
  pool[dbPool, poolClose],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/data/convert_col_types[convert_col_types],
  app/logic/data/data_connection[DataConnectionInterface],
  app/logic/interventions/intervention_combo_filter[build_combo_where_clause],
)

#' R6 class for SQLite database connections with connection pooling
#'
#' @description Manages SQLite database connections including:
#' - Connection pooling for efficient database access
#' - Data filtering and retrieval with type conversion
#' - Scenario metadata retrieval (NSP and BAU scenarios)
#' - Intervention column and administrative division data
#' - Age groups, risk strata, and year ranges
#' - Database performance optimization with indexes
#'
#' @field pool Database connection pool object
#' @field table_name Name of the main data table
#' @export
DataConnectionSQLite <- R6Class(
  classname = "DataConnectionSQLite",
  inherit = DataConnectionInterface,
  public = list(
    pool = NULL,
    table_name = NULL,
    #' Initialize a new SQLite database connection
    #'
    #' @param db_path Path to the SQLite database file
    #' @param table_name Name of the main data table (default: "data")
    #' @param ... Additional arguments.
    initialize = function(db_path, table_name = "data", ...) {
      stopifnot(
        "db_path must be a string" = qtest(db_path, "S1") && db_path != ""
      )
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite")
      super$uri <- db_path
      self$table_name <- table_name
      private$get_connection(log_ns = log_ns)
      private$init_database_setup(log_ns = log_ns)
    },
    #' Close the database connection pool
    #'
    #' Runs PRAGMA optimize and closes the connection pool.
    #' Should be called when the application shuts down.
    #'
    #' @param ... Additional arguments.
    shutdown = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:shutdown")

      log_debug("Closing DB connection pool ...", namespace = log_ns_fn(log_ns))

      dbExecute(self$pool, "PRAGMA optimize;")
      poolClose(self$pool)
      log_debug(
        "Closing DB connection pool ... done",
        namespace = log_ns_fn(log_ns)
      )
    },
    #' Filter data from an SQLite database
    #'
    #' @description This function connects to an SQLite database, constructs a
    #' query based on the provided parameters, retrieves the data, and then
    #' converts column types according to specified rules.
    #'
    #' @param columns Vector with column names to return.
    #' @param source Name of the table or view.
    #' @param filters A named list of filters where:
    #'   - Each name corresponds to a column to filter
    #'   - Each value is a vector of values to include
    #' @param conversion_rules A list or data structure defining rules for
    #'   column type conversion. Passed to `convert_col_types` function.
    #' @param ... Additional arguments.
    #'
    #' @return A data.table object containing the filtered and type-converted
    #    data.
    get_filtered_data = function(
      source = "data",
      columns = "*",
      filters = list(),
      conversion_rules,
      ...
    ) {
      stopifnot(
        "columns must be a string or vector of strings" = qtest(columns, "S+")
      )
      stopifnot("filters must be a list" = qtest(filters, "l*"))
      stopifnot(
        "conversion_rules must be a non-empty list" = qtest(
          conversion_rules,
          "L+"
        )
      )
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:get_filtered_data")

      log_debug("Connecting to DB", namespace = log_ns_fn(log_ns))

      # Establish a connection to the SQLite database
      con <- self$pool

      data <- {
        # Translate * to explicit column names
        columns <- if (length(columns) == 1 && columns == "*") {
          dbGetQuery(
            con,
            glue_sql("PRAGMA table_info({source});", .con = con)
          )$name
        } else {
          columns
        }

        if (length(filters) > 0) {
          # Build WHERE conditions using glue_sql
          conditions <- character(0)

          for (col in names(filters)) {
            values <- filters[[col]]
            conditions <- c(
              conditions,
              glue_sql("{`col`} IN ({values*})", .con = con)
            )
          }

          # Build the complete query
          where_clause <- glue_sql(
            paste("WHERE", paste(conditions, collapse = " AND "))
          )

          query <- glue_sql(
            "SELECT {`columns`*} FROM {source} {where_clause}",
            .con = con
          )
        } else {
          query <- glue_sql("SELECT {`columns`*} FROM {source}", .con = con)
        }

        log_debug("Sending query", namespace = log_ns_fn(log_ns))
        log_trace("{query}", namespace = log_ns_fn(log_ns))

        # Execute the SQL query with the specified parameters
        data <- dbGetQuery(con, query)
        data
      }

      log_debug("Converting data ...", namespace = log_ns_fn(log_ns))

      # Convert the retrieved data.frame to a data.table
      setDT(data)
      if (nrow(data) == 0) {
        warning("Data to convert has 0 rows. This may cause issues downstream")
        return(data)
      }

      # Convert column types based on the provided conversion rules
      data <- convert_col_types(data, conversion_rules)

      log_debug("Converting data ... done", namespace = log_ns_fn(log_ns))

      # Return the processed data
      data
    },

    #' Fetch data for specific intervention boolean combinations
    #'
    #' Optimized query method that filters data by intervention combinations
    #' (e.g., counterfactual scenarios). Combines standard filters with
    #' intervention-specific WHERE conditions.
    #'
    #' @param source Table or view name to query (default: "data")
    #' @param columns Column names to select (default: "*")
    #' @param filters Named list of standard filters (age_group, admin_2, etc.)
    #' @param intervention_combos Named list of named logical vectors representing
    #'   intervention combinations to match. Each logical vector should have
    #'   names matching intervention column names (e.g., deployed_int_CM).
    #' @param conversion_rules Named list mapping regex patterns to conversion functions
    #' @param ... Additional arguments (e.g., log_ns for logging namespace)
    #'
    #' @return data.table with rows matching the filters AND any of the intervention combinations
    #'
    #' @examples
    #' \dontrun{
    #' combos <- list(
    #'   original = c(deployed_int_CM = TRUE, deployed_int_IRS = TRUE),
    #'   cf1 = c(deployed_int_CM = TRUE, deployed_int_IRS = FALSE)
    #' )
    #' data <- connection$get_counterfactual_data(
    #'   filters = list(admin_2 = c("District1", "District2")),
    #'   intervention_combos = combos,
    #'   conversion_rules = rules
    #' )
    #' }
    get_counterfactual_data = function(
      source = "data",
      columns = "*",
      filters = list(),
      intervention_combos,
      conversion_rules,
      ...
    ) {
      stopifnot(
        "columns must be a string or vector of strings" = qtest(columns, "S+")
      )
      stopifnot("filters must be a list" = qtest(filters, "l*"))
      stopifnot("intervention_combos must be a list" = is.list(intervention_combos))
      stopifnot(
        "conversion_rules must be a non-empty list" = qtest(
          conversion_rules,
          "L+"
        )
      )
      args <- list(...)
      log_ns <- log_ns_builder(
        args,
        "DataConnectionSQLite:get_counterfactual_data"
      )

      log_debug("Connecting to DB", namespace = log_ns_fn(log_ns))

      # Establish a connection to the SQLite database
      con <- self$pool

      data <- {
        # Translate * to explicit column names
        columns <- if (length(columns) == 1 && columns == "*") {
          dbGetQuery(
            con,
            glue_sql("PRAGMA table_info({source});", .con = con)
          )$name
        } else {
          columns
        }

        # Build standard WHERE conditions
        standard_conditions <- character(0)
        if (length(filters) > 0) {
          for (col in names(filters)) {
            values <- filters[[col]]
            standard_conditions <- c(
              standard_conditions,
              glue_sql("{`col`} IN ({values*})", .con = con)
            )
          }
        }

        # Build intervention combination WHERE clause
        combo_clause <- build_combo_where_clause(intervention_combos, con)

        # Combine conditions
        where_parts <- character(0)
        if (length(standard_conditions) > 0) {
          where_parts <- c(where_parts, paste(standard_conditions, collapse = " AND "))
        }
        if (nchar(combo_clause) > 0) {
          where_parts <- c(where_parts, combo_clause)
        }

        # Build complete query
        if (length(where_parts) > 0) {
          where_clause <- glue_sql(
            paste("WHERE", paste(where_parts, collapse = " AND "))
          )
          query <- glue_sql(
            "SELECT {`columns`*} FROM {source} {where_clause}",
            .con = con
          )
        } else {
          query <- glue_sql("SELECT {`columns`*} FROM {source}", .con = con)
        }

        log_debug("Sending query", namespace = log_ns_fn(log_ns))
        log_trace("{query}", namespace = log_ns_fn(log_ns))

        # Execute the SQL query with the specified parameters
        data <- dbGetQuery(con, query)
        data
      }

      log_debug("Converting data ...", namespace = log_ns_fn(log_ns))

      # Convert the retrieved data.frame to a data.table
      setDT(data)
      if (nrow(data) == 0) {
        warning("Data to convert has 0 rows. This may cause issues downstream")
        return(data)
      }

      # Convert column types based on the provided conversion rules
      data <- convert_col_types(data, conversion_rules)

      log_debug("Converting data ... done", namespace = log_ns_fn(log_ns))

      # Return the processed data
      data
    },

    #' Retrieve all BAU and NSP scenario names from database
    #'
    #' Returns a vector containing all scenario names.
    all_scenarios = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:all_scenarios")

      log_debug("Retrieving 'all_scenarios' ...", namespace = log_ns_fn(log_ns))
      con <- self$pool
      result <- dbGetQuery(
        con,
        glue_sql(
          "SELECT * FROM all_scenarios",
          .con = con
        )
      )
      log_debug(
        "Retrieving 'all_scenarios' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result$scenario_name
    },

    #' Retrieve all NSP scenario names from database
    #'
    #' Returns a vector containing all NSP scenario names.
    scenarios = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:scenarios")

      log_debug("Retrieving 'scenarios' ...", namespace = log_ns_fn(log_ns))
      con <- self$pool
      result <- dbGetQuery(
        con,
        glue_sql(
          "SELECT * FROM scenarios",
          .con = con
        )
      )
      log_debug(
        "Retrieving 'scenarios' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result$scenario_name
    },

    #' Retrieve all BAU scenario names used as references
    #'
    #' Returns a vector containing all BAU scenario names.
    references = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:references")

      log_debug(
        "Retrieving 'references' ...",
        namespace = log_ns_fn(log_ns)
      )
      con <- self$pool
      result <- dbGetQuery(
        con,
        glue_sql(
          "SELECT * FROM references_view",
          .con = con
        )
      )
      log_debug(
        "Retrieving 'references' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result$scenario_name
    },

    #' Retrieve intervention column names from database
    #'
    #' Returns a vector containing all intervention column names.
    intervention_cols = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:intervention_cols")

      log_debug(
        "Retrieving 'intervention_cols' ...",
        namespace = log_ns_fn(log_ns)
      )
      con <- self$pool
      result <- grep(
        "^deployed_int_",
        dbGetQuery(con, "PRAGMA table_info(data);")$name,
        value = TRUE
      )
      log_debug(
        "Retrieving 'intervention_cols' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result
    },

    #' Retrieve unique administrative divisions from database
    #'
    #' Returns a table containing the admin_1 and admin_2 columns.
    admins = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:admins")

      log_debug("Retrieving 'admins' ...", namespace = log_ns_fn(log_ns))
      con <- self$pool
      result <- dbGetQuery(
        con,
        "SELECT * FROM admins"
      )
      log_debug("Retrieving 'admins' ... done", namespace = log_ns_fn(log_ns))
      result
    },

    #' Retrieve unique risk strata from database
    #'
    #' Returns a vector containing all risk strata.
    risk_strata = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:risk_strata")

      log_debug("Retrieving 'risk_strata' ...", namespace = log_ns_fn(log_ns))
      con <- self$pool
      result <- dbGetQuery(
        con,
        "SELECT * FROM risk_strata"
      )
      log_debug(
        "Retrieving 'risk_strata' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result$risk_stratum
    },

    #' Retrieve unique age groups from database
    #'
    #' Returns a vector containing all age groups.
    age_groups = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:age_groups")

      log_debug("Retrieving 'age_groups' ...", namespace = log_ns_fn(log_ns))
      con <- self$pool
      result <- dbGetQuery(
        con,
        "SELECT * FROM age_groups"
      )
      log_debug(
        "Retrieving 'age_groups' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result$age_group
    },

    #' Retrieve ordered year range from database
    #'
    #' Returns a vector containing all years.
    year_range = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "DataConnectionSQLite:year_range")

      log_debug("Retrieving 'year_range' ...", namespace = log_ns_fn(log_ns))
      con <- self$pool
      result <- dbGetQuery(
        con,
        "SELECT * FROM year_range"
      )
      log_debug(
        "Retrieving 'year_range' ... done",
        namespace = log_ns_fn(log_ns)
      )
      result$year
    },

    #' @description Retrieve pre-aggregated district-level summaries for the
    #'   Economic Evaluation module. Performs full aggregation in SQLite,
    #'   collapsing all year and age_group dimensions (one row per scenario ×
    #'   district × EIR_CI).
    #'
    #' @param intervention_cols Character vector of deployed_int_* column names
    #'   (e.g. c("deployed_int_CM", "deployed_int_ICCM", ...)).
    #' @param age_groups Character vector of age groups to include.
    #' @param year_start Integer. First year of the evaluation period.
    #' @param year_end Integer. Last year of the evaluation period.
    #' @param filters Optional named list of additional WHERE conditions.
    #' @param ... Additional arguments (must include log_ns for logging).
    #'
    #' @return A data.table grouped by (scenario_name, plan, admin_2, EIR_CI)
    #'   with columns: vol_* (total intervention volumes across the year range,
    #'   for cost = vol * unit_cost / n_years), cum_cases_end, cum_cases_start
    #'   (for averted_period = end - start), and deployed_int_* (flags).
    get_ee_district_summaries = function(
      intervention_cols,
      age_groups,
      year_start,
      year_end,
      filters = list(),
      ...
    ) {
      args <- list(...)
      log_ns <- log_ns_builder(
        args,
        "DataConnectionSQLite:get_ee_district_summaries"
      )
      con <- self$pool

      # Derive intervention short names from deployed_int_* column names
      int_names <- sub("^deployed_int_", "", intervention_cols)

      n_years <- year_end - year_start + 1L

      # Cost sub-query: volumes across the evaluation year range. SUM across all
      # rows in year range, then divide by n_years to get the average yearly
      # cost (matching: mean(sum(cost_per_year))).
      vol_exprs <- vapply(
        int_names,
        function(int) {
          dep <- paste0("deployed_int_", int)
          cov <- paste0("coverage_int_", int)
          paste0(
            "SUM(nHost * ",
            dep,
            " * ",
            cov,
            ") / ",
            n_years,
            ".0 AS vol_",
            int
          )
        },
        character(1)
      )

      flag_exprs <- vapply(
        intervention_cols,
        function(col) paste0("MAX(", col, ") AS ", col),
        character(1)
      )

      # Cases: AVG(cum_nUncomp) at year_end and year_start - 1. Use conditional
      # aggregation to get both in one pass.
      cases_end_expr <- glue_sql(
        "AVG(CASE WHEN year = {year_end} THEN cum_nUncomp END) AS cum_cases_end",
        .con = con
      )
      cases_start_expr <- glue_sql(
        paste0(
          "COALESCE(AVG(CASE WHEN year = {year_start - 1L} THEN cum_nUncomp END), 0)",
          " AS cum_cases_start"
        ),
        .con = con
      )

      select_cols <- paste(
        c(
          "scenario_name",
          "plan",
          "admin_2",
          "EIR_CI",
          vol_exprs,
          cases_end_expr,
          cases_start_expr,
          flag_exprs
        ),
        collapse = ",\n  "
      )

      group_cols <- "scenario_name, plan, admin_2, EIR_CI"

      # WHERE: age_group filter + year range (need year_start-1 for cases
      # baseline) + optional extras
      year_min <- year_start - 1L
      all_filters <- c(list(age_group = age_groups), filters)
      conditions <- glue_sql(
        "year BETWEEN {year_min} AND {year_end}",
        .con = con
      )
      for (col in names(all_filters)) {
        values <- all_filters[[col]]
        conditions <- c(
          conditions,
          glue_sql("{`col`} IN ({values*})", .con = con)
        )
      }
      where_clause <- paste("WHERE", paste(conditions, collapse = " AND "))

      query <- paste0(
        "SELECT\n  ",
        select_cols,
        "\n",
        "FROM data\n",
        where_clause,
        "\n",
        "GROUP BY ",
        group_cols
      )

      log_debug(
        "Fetching EE district summaries ...",
        namespace = log_ns_fn(log_ns)
      )
      log_trace("{query}", namespace = log_ns_fn(log_ns))

      result <- dbGetQuery(con, query)
      setDT(result)

      # Convert deployed_int_* flags from integer (0/1) to logical
      for (col in intervention_cols) {
        if (col %in% names(result)) {
          result[, (col) := as.logical(get(col))]
        }
      }

      log_debug(
        "Fetching EE district summaries ... done ({nrow(result)} rows)",
        namespace = log_ns_fn(log_ns)
      )

      result
    }
  ),
  private = list(
    #' Get connection pool
    #'
    #' Returns the connection pool object suitable for DBI functions
    #' (dbGetQuery, dbExecute, etc.). The pool is stored and
    #' reused across requests.
    #'
    #' @param ... Additional arguments.
    #' @return A Pool object that can be used with DBI functions
    get_connection = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "get_connection")

      if (is.null(self$pool)) {
        log_debug(
          "Creating new DB connection pool...",
          namespace = log_ns_fn(log_ns)
        )

        pool_obj <- dbPool(
          drv = SQLite(),
          dbname = super$uri,
          # Connection validation query
          validateQuery = "SELECT 1",
          # Connection setup - apply pragmas to each connection
          onCreate = function(con) {
            dbExecute(con, "PRAGMA journal_mode = WAL;")
            dbExecute(con, "PRAGMA busy_timeout = 5000;")
            dbExecute(con, "PRAGMA synchronous = normal;")
            dbExecute(con, "PRAGMA temp_store = memory;")
            dbExecute(con, "PRAGMA mmap_size = 30000000000;")
            dbExecute(con, "PRAGMA page_size = 32768;")
            dbExecute(con, "PRAGMA cache_size = -65536;")
          }
        )

        self$pool <- pool_obj
        log_debug(
          "Creating new DB connection pool ... done",
          namespace = log_ns_fn(log_ns)
        )
      } else {
        log_debug(
          "Reusing existing DB connection pool",
          namespace = log_ns_fn(log_ns)
        )
      }
    },
    #' Initialize database with performance indexes and views
    #'
    #' Checks if performance indexes and views exist and creates them if
    #' missing. This is idempotent - safe to run multiple times.
    #'
    #' @param ... Additional arguments.
    #' @return TRUE if indexes were created, FALSE if they already existed
    init_database_setup = function(...) {
      args <- list(...)
      log_ns <- log_ns_builder(
        args,
        "init_database_setup"
      )

      con <- self$pool

      # Ensure views exist
      log_info("Ensuring DB views exist ...", namespace = log_ns_fn(log_ns))

      tryCatch(
        {
          dbExecute(
            con,
            glue_sql(
              "CREATE VIEW IF NOT EXISTS all_scenarios AS
SELECT DISTINCT scenario_name FROM data WHERE plan IN ({unname(unlist(lapply(config$get(\"plans\"), names)))*})",
              .con = con
            )
          )
          dbExecute(
            con,
            glue_sql(
              "CREATE VIEW IF NOT EXISTS scenarios AS
SELECT DISTINCT scenario_name FROM data WHERE plan IN ({names(config$get(\"plans\")[[\"scenarios\"]])*})",
              .con = con
            )
          )
          dbExecute(
            con,
            glue_sql(
              "CREATE VIEW IF NOT EXISTS references_view AS
SELECT DISTINCT scenario_name FROM data WHERE plan IN ({names(config$get(\"plans\")[[\"references\"]])*})",
              .con = con
            )
          )
          dbExecute(
            con,
            "CREATE VIEW IF NOT EXISTS admins AS
SELECT DISTINCT admin_1, admin_2 FROM data"
          )
          dbExecute(
            con,
            "CREATE VIEW IF NOT EXISTS risk_strata AS
SELECT DISTINCT risk_stratum FROM data"
          )
          dbExecute(
            con,
            "CREATE VIEW IF NOT EXISTS age_groups AS
SELECT DISTINCT age_group FROM data"
          )
          dbExecute(
            con,
            "CREATE VIEW IF NOT EXISTS year_range AS
SELECT DISTINCT year FROM data ORDER BY year ASC"
          )

          intervention_cols <- self$intervention_cols(log_ns = log_ns)
          dbExecute(
            con,
            glue_sql(
              "CREATE VIEW IF NOT EXISTS int_data_pool AS
SELECT DISTINCT scenario_name, admin_1, admin_2, risk_stratum, age_group, year,
{`intervention_cols`*} FROM data",
              .con = con
            )
          )
        },
        error = function(e) {
          log_error(
            "Error creating database views: {e$message}",
            namespace = log_ns_fn(log_ns)
          )
          stop("Error creating database views")
        }
      )

      log_info(
        "Ensuring DB views exist ... done",
        namespace = log_ns_fn(log_ns)
      )

      log_info("Checking database indexes ...", namespace = log_ns_fn(log_ns))

      # Check if indexes already exist
      existing_indexes <- dbGetQuery(
        con,
        "SELECT COUNT(*) as count FROM sqlite_master
     WHERE type='index' AND tbl_name='data'"
      )$count

      # Expected number of indexes
      expected_indexes <- 9

      if (existing_indexes == expected_indexes) {
        log_info(
          "Database already optimized ({existing_indexes} indexes present)",
          namespace = log_ns_fn(log_ns)
        )
        return(FALSE)
      }

      if (existing_indexes > 0 && existing_indexes < expected_indexes) {
        log_warn(
          "Found {existing_indexes} indexes, expected {expected_indexes}. ",
          "Will create missing indexes.",
          namespace = log_ns_fn(log_ns)
        )
      }

      # Create indexes
      log_info(
        "Creating database indexes for performance optimization. ",
        "This will take some time on first startup...",
        namespace = log_ns_fn(log_ns)
      )

      start_time <- Sys.time()

      tryCatch(
        {
          # Create single-column indexes
          dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_plan ON data(plan);")
          log_debug("Created idx_plan", namespace = log_ns_fn(log_ns))

          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_scenario_name ON data(scenario_name);"
          )
          log_debug("Created idx_scenario_name", namespace = log_ns_fn(log_ns))

          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_admin_1 ON data(admin_1);"
          )
          log_debug("Created idx_admin_1", namespace = log_ns_fn(log_ns))

          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_admin_2 ON data(admin_2);"
          )
          log_debug("Created idx_admin_2", namespace = log_ns_fn(log_ns))

          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_age_group ON data(age_group);"
          )
          log_debug("Created idx_age_group", namespace = log_ns_fn(log_ns))

          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_risk_stratum ON data(risk_stratum);"
          )
          log_debug("Created idx_risk_stratum", namespace = log_ns_fn(log_ns))

          dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_year ON data(year);")
          log_debug("Created idx_year", namespace = log_ns_fn(log_ns))

          # Create composite indexes
          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_plan_scenario
         ON data(plan, scenario_name);"
          )
          log_debug("Created idx_plan_scenario", namespace = log_ns_fn(log_ns))

          dbExecute(
            con,
            "CREATE INDEX IF NOT EXISTS idx_admin_combo
         ON data(admin_1, admin_2);"
          )
          log_debug("Created idx_admin_combo", namespace = log_ns_fn(log_ns))

          # Update query planner statistics
          log_debug(
            "Running ANALYZE to update query planner statistics...",
            namespace = log_ns_fn(log_ns)
          )
          dbExecute(con, "ANALYZE;")

          end_time <- Sys.time()
          elapsed <- round(
            as.numeric(difftime(end_time, start_time, units = "secs"))
          )

          # Verify all indexes were created
          final_count <- dbGetQuery(
            con,
            "SELECT COUNT(*) as count FROM sqlite_master
         WHERE type='index' AND tbl_name='data'"
          )$count

          if (final_count == expected_indexes) {
            log_info(
              "Successfully created {expected_indexes} database indexes ",
              "in {elapsed} seconds. Subsequent startups will be faster.",
              namespace = log_ns_fn(log_ns)
            )
            return(TRUE)
          } else {
            log_warn(
              "Created indexes, but found {final_count} instead of \\
{expected_indexes}",
              namespace = log_ns_fn(log_ns)
            )
            return(TRUE)
          }
        },
        error = function(e) {
          log_warn(
            "Error creating database indexes: {e$message}",
            namespace = log_ns_fn(log_ns)
          )
          log_warn(
            "App will continue but performance may be degraded",
            namespace = log_ns_fn(log_ns)
          )
          return(FALSE)
        }
      )
    }
  )
)
