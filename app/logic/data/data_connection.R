box::use(
  R6[R6Class],
)

# fmt: skip
box::use(
  app/logic/data/data_connection_sqlite[DataConnectionSQLite],
)


#' R6 base class for data connections
#'
#' @description Provides a base interface for data connections including:
#' - URI storage for data source location (file path or URL)
#' - Inheritance for specific connection types (SQLite, REST, etc.)
#' - Common interface for data retrieval operations
#'
#' @field uri Path or URL to the data source
#' @export
DataConnectionInterface <- R6Class(
  classname = "DataConnectionInterface",
  public = list(
    # URI is path or URL
    uri = NULL,

    # Methods
    initialize = function(db_path, ...) {
      stop("This class cannot be instantiated")
    },
    shutdown = function(...) {
      stop("Not implemented")
    },
    get_filtered_data = function(
      source,
      columns = "*",
      filters = list(),
      conversion_rules,
      ...
    ) {
      stop("Not implemented")
    },
    all_scenarios = function(...) {
      stop("Not implemented")
    },
    scenarios = function(...) {
      stop("Not implemented")
    },
    references = function(...) {
      stop("Not implemented")
    },
    intervention_cols = function(...) {
      stop("Not implemented")
    },
    admins = function(...) {
      stop("Not implemented")
    },
    risk_strata = function(...) {
      stop("Not implemented")
    },
    age_groups = function(...) {
      stop("Not implemented")
    },
    year_range = function(...) {
      stop("Not implemented")
    },
    get_ee_district_summaries = function(
      intervention_cols,
      age_groups,
      year_start,
      year_end,
      filters = list(),
      ...
    ) {
      stop("Not implemented")
    }
  )
)

#' Factory function for creating data connections
#'
#' @description Creates a data connection object based on the specified type.
#'   Supported connection types include:
#'
#' - SQLite: Local SQLite database connections with connection pooling
#' - Supabase: Supabase database connections (not yet implemented)
#'
#' @param type Character string specifying the connection type. Must be one of
#'   "SQLite" or "Supabase". Defaults to "SQLite".
#' @param ... Additional arguments passed to the specific connection class
#'   constructor. For SQLite, this includes `db_path` (path to the database
#'   file).
#'
#' @return An R6 object inheriting from `DataConnectionInterface` that provides
#'   methods for data retrieval, filtering, and database operations.
#'
#' @export
make_data_connection <- function(type = c("SQLite", "Supabase"), ...) {
  type <- match.arg(type)
  switch(
    type,
    SQLite = DataConnectionSQLite$new(...),
    Supabase = {
      stop("Not implemented.")
    }
    # TODO 2026-02-06: Coming soon.
    # Supabase = DataConnectionSupabase$new(...)
  )
}
