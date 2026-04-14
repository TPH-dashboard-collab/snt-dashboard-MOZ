box::use(
  config,
  logger[log_debug],
  shiny,
)

# fmt: skip
box::use(
  app/logic/visualization/tables[format_impact_table],
  app/logic/core/logging[log_ns_builder, log_ns_fn],
)

# UI ---------------------------------------------------------------------------

#' Creates the user interface for cumulative relative impact visualization
#'
#' This function generates a Shiny UI component that displays an interactive
#' Plotly chart showing the relative impact of intervention scenarios compared
#' to a reference baseline.
#'
#' @param id The module ID used for namespacing UI elements
#' @return A Shiny UI component containing an interactive Plotly visualization
#' @export
ui <- function(id) {
  # Create a namespace function for this module
  ns <- shiny$NS(id)

  shiny$tableOutput(ns("table"))
}

# Server -----------------------------------------------------------------------

#' Creates the server logic for the intervention map
#'
#' @param id The module ID used for namespacing between different instances of
#'   the module
#' @param variables A list containing user selections and configuration:
#'   * session_state: list with age_group, agg_level, reference, etc.
#'   * intervention_cols: character vector of intervention column names
#'   * db_path: path to database
#'   * conversion_rules: data conversion rules
#' @param scenario_name Character string specifying the scenario to display
#' @param opts List of optional parameters:
#'   * colors: Custom color vector for scenarios
#' @param year_start Numeric value indicating the start year of the range
#' @param year_end Numeric value indicating the end year of the range
#' @param trigger Reactive expression to trigger plot updates
#' @param scenarios Character vector of scenario names to include. Defaults to
#'   variables$scenarios. For custom data, use c("custom").
#' @return A Shiny server function that handles map rendering and updates
#' @export
server <- function(
  id,
  variables,
  use_custom_data = FALSE,
  year_start,
  year_end,
  trigger,
  scenarios = NULL,
  ...
) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "relative_impact_table")

      # Reactive for data preparation
      table_data <- shiny$reactive({
        # Establish dependency on trigger
        if (use_custom_data) {
          shiny$req(
            variables$session_state$custom_data,
            ncol(variables$session_state$custom_data) > 1
          )
          variables$custom_data_trigger()
        } else {
          trigger()
        }

        # fmt: skip
        box::use(
          app/logic/core/utils[aggregate_impact],
        )

        # Get year values (handle both reactive and static values)
        year_start_val <- if (shiny$is.reactive(year_start)) {
          year_start()
        } else {
          year_start
        }
        year_end_val <- if (shiny$is.reactive(year_end)) {
          year_end()
        } else {
          year_end
        }

        # Ensure year values are available
        shiny$req(year_start_val, year_end_val)

        log_debug(
          "Fetching data ...",
          namespace = log_ns_fn(log_ns)
        )
        # Get data - don't filter by year here, aggregate_impact handles it
        filters <- list(
          age_group = variables$session_state$age_group,
          plan = unname(unlist(lapply(config$get("plans"), names))),
          scenario_name = c(
            variables$scenarios,
            variables$session_state$reference
          )
        )
        if (
          variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
            length(variables$session_state$region_selected) > 0
        ) {
          filters <- c(
            filters,
            list(
              admin_1 = variables$session_state$region_selected
            )
          )
        }
        if (
          variables$session_state$agg_level == "Risk Strata" &&
            length(variables$session_state$strata_selected) > 0
        ) {
          filters <- c(
            filters,
            list(risk_stratum = variables$session_state$strata_selected)
          )
        }

        col_names <- c(
          "scenario_name",
          "admin_1",
          "admin_2",
          "EIR_CI",
          "seed",
          "cum_nUncomp",
          "cum_nSevere",
          "cum_tUncomp",
          "cum_tSevere",
          "cum_expectedDirectDeaths",
          "year"
        )

        dt <- if (use_custom_data) {
          variables$session_state$custom_data[, ..col_names]
        } else {
          variables$data_con$get_filtered_data(
            columns = col_names,
            filters = filters,
            conversion_rules = variables$conversion_rules,
            log_ns = log_ns
          )
        }
        log_debug(
          "Fetching data ... done",
          namespace = log_ns_fn(log_ns)
        )

        log_debug(
          "Aggregating data ...",
          namespace = log_ns_fn(log_ns)
        )
        # Aggregate impact for the year range
        dt <- aggregate_impact(dt, year_start_val, year_end_val)

        log_debug(
          "Aggregating data ... done",
          namespace = log_ns_fn(log_ns)
        )

        dt
      })

      output$table <- shiny$renderTable(
        {
          log_debug(
            "Rendering relative impact table table",
            namespace = log_ns_fn(log_ns)
          )
          data <- table_data()
          # Use provided scenarios or default to variables$scenarios
          scenario_list <- if (is.null(scenarios)) {
            variables$scenarios
          } else {
            scenarios
          }

          # Format the impact data for display in the table
          format_impact_table(
            # Aggregated impact data for the specified year
            impact_data = data,
            # Use the reference scenario for comparison
            reference = variables$session_state$reference,
            # Include the specified scenarios
            scenarios = scenario_list,
            # Use the impact & scenario mapping for display names
            impact_mapping = variables$impact_mapping,
            scenario_mapping = variables$scenario_mapping
          )
        },
        # Table display options:
        # Add borders around table cells
        bordered = TRUE,
        # Enable hover effects
        hover = TRUE,
        # Use large spacing between cells
        spacing = "l",
        # Preserve special characters
        sanitize.text.function = identity,
        # Center-align table content
        align = "c"
      )
    }
  )
}
