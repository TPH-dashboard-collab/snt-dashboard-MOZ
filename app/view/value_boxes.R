box::use(
  bsicons[bs_icon],
  bslib[layout_column_wrap, value_box, value_box_theme],
  config,
  data.table[`%chin%`],
  logger[log_debug],
  scales[comma, dollar],
  shiny,
)

#' Format large numbers in human-readable form
#'
#' @param x Numeric value to format
#' @param prefix Prefix to add (default: "$")
#' @param threshold Minimum value for abbreviation (default: 1e6)
#' @return Formatted string (e.g., "$1.5M", "$250K", "1,234")
format_large_number <- function(x, prefix = "$", threshold = 1e6) {
  if (is.na(x) || !is.finite(x)) {
    return("N/A")
  }

  abs_x <- abs(x)
  sign_str <- if (x < 0) "-" else ""

  if (abs_x >= 1e9) {
    formatted <- sprintf("%.1fB", abs_x / 1e9)
    paste0(sign_str, prefix, formatted)
  } else if (abs_x >= threshold && abs_x >= 1e6) {
    formatted <- sprintf("%.1fM", abs_x / 1e6)
    paste0(sign_str, prefix, formatted)
  } else if (abs_x >= threshold && abs_x >= 1e3) {
    formatted <- sprintf("%.0fK", abs_x / 1e3)
    paste0(sign_str, prefix, formatted)
  } else {
    # For smaller numbers, use comma formatting
    paste0(sign_str, prefix, comma(round(abs_x)))
  }
}

# UI ---------------------------------------------------------------------------

#' Creates the user interface for displaying summary value boxes
#'
#' This function generates a Shiny UI component that displays three value boxes
#' showing key summary metrics: total cases averted, total program cost, and
#' cost per case averted.
#'
#' @param id The module ID used for namespacing UI elements
#' @return A Shiny UI component containing three value boxes in a row
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$uiOutput(ns("value_boxes_ui"))
}

# Server -----------------------------------------------------------------------

#' Creates the server logic for the value boxes
#'
#' @param id The module ID used for namespacing between different instances of
#'   the module
#' @param variables A list containing user selections and configuration:
#'   * session_state: list with age_group, agg_level, reference, etc.
#'   * intervention_cols: character vector of intervention column names
#'   * db_path: path to database
#'   * conversion_rules: data conversion rules
#' @param use_custom_data Logical, whether to use custom data (panel 3) or
#'   database data (panel 2)
#' @param year_start Numeric value indicating the start year of the range
#' @param year_end Numeric value indicating the end year of the range
#' @param trigger Reactive expression to trigger updates
#' @return A Shiny server function that handles value box rendering and updates
#' @export
server <- function(
  id,
  variables,
  use_custom_data = FALSE,
  year_start,
  year_end,
  trigger
) {

  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      # Reactive for data preparation and metric calculation
      metrics_data <- shiny$reactive({
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
          app/logic/interventions/colnames_to_interventions[colnames_to_interventions],
          app/logic/analytics/costs[calculate_cost_effectiveness, calculate_intervention_costs],
          app/logic/data/data_connection_sqlite[DataConnectionSQLite],
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
          "Fetching data for value boxes ...",
          namespace = "namespace:value_boxes"
        )

        # Determine scenarios to use
        scenarios_to_use <- if (use_custom_data) {
          "custom"
        } else {
          variables$scenarios
        }

        # Build filters - NO year filter here since cost calculation needs all
        # years. Year filtering is done in aggregate_impact for impact metrics.
        filters <- list(
          age_group = variables$session_state$age_group,
          plan = unname(unlist(lapply(config$get("plans"), names))),
          scenario_name = c(
            scenarios_to_use,
            variables$session_state$reference
          )
        )

        # Add region/strata filters if applicable
        if (
          variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
            length(variables$session_state$region_selected) > 0
        ) {
          filters <- c(
            filters,
            list(admin_1 = variables$session_state$region_selected)
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

        # Fetch data - include all years for cost calculation
        dt <- if (use_custom_data) {
          variables$session_state$custom_data
        } else {
          variables$data_con$get_filtered_data(
            filters = filters,
            conversion_rules = variables$conversion_rules
          )
        }

        log_debug(
          "Fetching data for value boxes ... done",
          namespace = "namespace:value_boxes"
        )

        log_debug(
          "Calculating value box metrics ...",
          namespace = "namespace:value_boxes"
        )

        # Calculate intervention costs
        dt_int_costs <- calculate_intervention_costs(
          dt = dt[
            scenario_name %chin%
              c(
                variables$session_state$reference,
                scenarios_to_use
              )
          ],
          interventions = colnames_to_interventions(dt),
          intervention_cols = grep(
            pattern = "^deployed_int_.*",
            x = colnames(dt),
            value = TRUE
          ),
          intervention_cov = grep(
            pattern = "^coverage_int_.*",
            x = colnames(dt),
            value = TRUE
          ),
          unit_costs = variables$session_state$unit_costs,
          year_start = year_start_val,
          year_end = year_end_val
        )

        # Calculate cost effectiveness
        dt_cost_eff <- calculate_cost_effectiveness(
          dt_int_costs,
          dt[
            scenario_name %chin%
              c(
                variables$session_state$reference,
                scenarios_to_use
              )
          ],
          variables$session_state$reference,
          year_start = year_start_val,
          year_end = year_end_val
        )

        # Calculate cases averted using aggregate_impact
        impact_data <- aggregate_impact(
          data = dt,
          year_start = year_start_val,
          year_end = year_end_val
        )

        # Cases averted = reference cumulative cases - scenario cumulative cases
        reference_cases <- impact_data[
          scenario_name == variables$session_state$reference,
          cum_nUncomp_mean
        ]
        scenario_cases <- impact_data[
          scenario_name %chin% scenarios_to_use,
          cum_nUncomp_mean
        ]
        cases_averted <- reference_cases - scenario_cases[1]

        # Extract metrics
        # Total cost for the non-reference scenario
        total_cost <- dt_int_costs[
          !startsWith(scenario_name, variables$session_state$reference),
          total_cost_mean
        ][1]

        # Cost per case averted
        cost_per_averted <- if (nrow(dt_cost_eff) > 0) {
          dt_cost_eff[1, cost_per_averted_mean]
        } else {
          NA_real_
        }

        log_debug(
          "Calculating value box metrics ... done",
          namespace = "namespace:value_boxes"
        )

        list(
          cases_averted = cases_averted,
          total_cost = total_cost,
          cost_per_averted = cost_per_averted
        )
      })

      # Render value boxes UI
      output$value_boxes_ui <- shiny$renderUI({
        shiny$req(metrics_data())
        metrics <- metrics_data()

        layout_column_wrap(
          width = 1 / 3,

          # Cases Averted value box
          value_box(
            title = shiny$tagList(
              "Total Cases Averted",
              shiny$tags$small(
                style = "font-size: 0.7em; opacity: 0.7; display: block;",
                "compared to reference"
              )
            ),
            value = format_large_number(
              round(metrics$cases_averted),
              prefix = ""
            ),
            showcase = bs_icon("heart-pulse"),
            theme = value_box_theme(bg = "white", fg = "#198754"),
            class = "border border-success"
          ),

          # Total Program Cost value box
          value_box(
            title = "Total Program Cost",
            value = format_large_number(metrics$total_cost, prefix = "$"),
            showcase = bs_icon("currency-dollar"),
            theme = value_box_theme(bg = "white", fg = "#e6a000"),
            class = "border border-warning"
          ),

          # Cost Per Case Averted value box
          value_box(
            title = "Cost Per Case Averted",
            value = if (is.finite(metrics$cost_per_averted)) {
              dollar(metrics$cost_per_averted, accuracy = 0.01)
            } else {
              "N/A"
            },
            showcase = bs_icon("calculator"),
            theme = value_box_theme(bg = "white", fg = "#0d6efd"),
            class = "border border-info"
          )
        )
      })
    }
  )
}