box::use(
  bslib[
    card,
    card_header,
    layout_column_wrap,
    nav_panel,
    navset_card_tab,
  ],
  config,
  logger[log_debug],
  plotly[plotlyOutput, renderPlotly],
  shiny,
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/visualization/plots[costs_averted_plot, costs_plot],
  app/logic/visualization/tables[format_costs_table],
)

# UI ---------------------------------------------------------------------------

#' Creates the user interface for displaying cost analysis results
#'
#' This function generates a Shiny UI component that displays cost analysis
#' results in a two-column layout. The left column contains interactive
#' visualizations of cumulative costs and cost-effectiveness metrics, while the
#' right column shows a detailed table of cost estimates.
#'
#' @param id The module ID used for namespacing UI elements
#' @return A Shiny UI component containing an interactive Plotly visualization
#' @export
ui <- function(id) {
  # Create a namespace function for this module
  ns <- shiny$NS(id)

  # Create a two-column layout with equal width
  layout_column_wrap(
    width = 1 / 2,

    # Left column: Tabbed card with cost visualizations (dynamic title)
    shiny$uiOutput(ns("costs_card")),

    # Right column: Card with cost estimates table (dynamic title)
    shiny$uiOutput(ns("costs_table_card"))
  )
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
#' @param year_start Numeric value indicating which year's range to display
#' @param year_end Numeric value indicating which year's range to display
#' @param trigger Reactive expression to trigger plot updates
#' @return A Shiny server function that handles map rendering and updates
#' @export
server <- function(
  id,
  variables,
  use_custom_data = FALSE,
  year_start,
  year_end,
  trigger,
  ...
) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "costs")

      # Reactive for data preparation
      costs_data <- shiny$reactive({
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
          app/logic/analytics/costs[
              calculate_cost_effectiveness,
              calculate_intervention_costs,
            ],
          app/logic/data/data_connection_sqlite[DataConnectionSQLite],
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
        # Get data - NO year filter here since cost calculation needs all years
        # in the range. Year filtering is done in calculate_intervention_costs
        # and aggregate_impact.
        filters <- list(
          age_group = variables$session_state$age_group,
          plan = unname(unlist(lapply(config$get("plans"), names))),
          scenario_name = c(
            if (use_custom_data) c("custom") else variables$scenarios,
            variables$session_state$reference
          )
        )
        if (
          variables$session_state$agg_level == "Regional" &&
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

        dt <- if (use_custom_data) {
          variables$session_state$custom_data
        } else {
          variables$data_con$get_filtered_data(
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

        dt_int_costs <- calculate_intervention_costs(
          dt = dt[
            scenario_name %chin%
              c(
                variables$session_state$reference,
                if (use_custom_data) c("custom") else variables$scenarios
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

        dt_cost_eff <- calculate_cost_effectiveness(
          dt_int_costs,
          dt[
            scenario_name %chin%
              c(
                variables$session_state$reference,
                if (use_custom_data) c("custom") else variables$scenarios
              )
          ],
          variables$session_state$reference,
          year_start = year_start_val,
          year_end = year_end_val
        )

        # Add human-readable scenario names
        temp_scenario_mapping <- variables$scenario_mapping
        temp_scenario_mapping[
          variables$session_state$reference
        ] <- "Reference"
        dt_int_costs <- dt_int_costs[,
          pretty_name := temp_scenario_mapping[scenario_name]
        ]

        dt_cost_eff <- dt_cost_eff[,
          pretty_name := variables$scenario_mapping[scenario_name]
        ]

        log_debug(
          "Aggregating data ... done",
          namespace = log_ns_fn(log_ns)
        )

        list(
          dt_int_costs = dt_int_costs,
          dt_cost_eff = dt_cost_eff,
          dt = dt
        )
      })

      # Helper to format year label
      year_label <- shiny$reactive({
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
        shiny$req(year_start_val, year_end_val)

        if (year_start_val == year_end_val) {
          paste0("in ", year_end_val)
        } else {
          paste0("in ", year_start_val, "-", year_end_val)
        }
      })

      # Render the costs card with dynamic title
      output$costs_card <- shiny$renderUI({
        ns <- session$ns
        navset_card_tab(
          title = paste0("Costing estimates (", year_label(), ")"),
          nav_panel(
            title = "Cumulative total costs",
            full_screen = TRUE,
            plotlyOutput(ns("costs_plot"))
          ),
          nav_panel(
            title = "Cumulative costs per case averted",
            full_screen = TRUE,
            plotlyOutput(ns("costs_averted_plot"))
          )
        )
      })

      # Render the costs table card with dynamic title
      output$costs_table_card <- shiny$renderUI({
        ns <- session$ns
        card(
          card_header(paste0("Costing estimates (", year_label(), ")")),
          full_screen = TRUE,
          shiny$tableOutput(ns("costs_table"))
        )
      })

      output$costs_plot <- renderPlotly({
        log_debug("Rendering costs plot", namespace = log_ns_fn(log_ns))
        data <- costs_data()$dt_int_costs
        costs_plot(costs_dt = data)
      })

      output$costs_averted_plot <- renderPlotly({
        log_debug("Rendering costs averted plot", namespace = log_ns_fn(log_ns))
        data <- costs_data()$dt_cost_eff
        costs_averted_plot(plot_dt = data)
      })

      output$costs_table <- shiny$renderTable(
        {
          log_debug("Rendering costs table", namespace = log_ns_fn(log_ns))
          cost_data <- costs_data()$dt_int_costs
          sim_data <- costs_data()$dt
          # Format table for display
          format_costs_table(
            cost_data,
            sim_data,
            shiny$isolate(variables$session_state$reference),
            if (use_custom_data) c("custom") else variables$scenarios,
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
