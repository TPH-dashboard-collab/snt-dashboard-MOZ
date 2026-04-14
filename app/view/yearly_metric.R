# This module provides UI and server components for creating interactive time
# series visualizations of simulation metrics.

box::use(
  config,
  logger[log_debug],
  plotly[plotlyOutput, renderPlotly],
  shiny[
    NS,
    isolate,
    moduleServer,
    reactive,
    req,
  ],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/core/utils[aggregate_metric],
  app/logic/visualization/plots[yearlyMetric_plot],
)

# UI ---------------------------------------------------------------------------

#' Create a Shiny UI component for yearly metric visualization
#'
#' @param id The module ID used for namespacing
#' @return A plotly output element wrapped in the module's namespace
#' @export
ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot"))
}

# Server -----------------------------------------------------------------------

#' Create a Shiny server component for yearly metric visualization
#'
#' This function creates a server module that processes simulation data and
#' generates an interactive plot showing metric trends over time for different
#' scenarios.
#'
#' @param id The module ID used for namespacing
#' @param variables Instance of [`Variables`] R6 class.
#' @param metric Character string specifying which column to use as the metric
#' @param opts List of optional parameters:
#'   * y_label: Custom label for y-axis
#'   * colors: Custom color vector for scenarios
#' @param trigger Reactive expression to trigger plot updates
#' @return A Shiny module server function that renders an interactive plotly
#'   visualization of the specified metric over time
#' @export
server <- function(
  id,
  variables,
  use_custom_data = FALSE,
  metric,
  opts,
  trigger,
  ...
) {
  moduleServer(id, function(input, output, session) {
    args <- list(...)
    log_ns <- log_ns_builder(args, "yearly_metric")

    # Reactive for data preparation
    plot_data <- reactive({
      # Establish dependency on trigger
      if (use_custom_data) {
        req(
          variables$session_state$custom_data,
          ncol(variables$session_state$custom_data) > 1
        )
        variables$custom_data_trigger()
      } else {
        trigger()
      }

      log_debug(
        "Fetching data ...",
        namespace = log_ns_fn(log_ns)
      )

      # Get data
      filters <- list(
        age_group = variables$session_state$age_group,
        plan = unname(unlist(lapply(config$get("plans"), names))),
        scenario_name = c(
          variables$scenarios,
          variables$session_state$reference
        ),
        year = variables$year_range
      )
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

      # Aggregate data based on the specified metric
      log_debug(
        "Aggregating data ...",
        namespace = log_ns_fn(log_ns)
      )
      agg_data <- aggregate_metric(dt, metric)
      log_debug(
        "Aggregating data ... done",
        namespace = log_ns_fn(log_ns)
      )
      agg_data
    })

    output$plot <- renderPlotly({
      log_debug("Rendering yearly metric plot", namespace = log_ns_fn(log_ns))
      data <- plot_data()
      yearlyMetric_plot(
        data,
        opts = opts,
        scenario_mapping = variables$scenario_mapping,
        scenarios = variables$scenarios,
        reference = isolate(variables$session_state$reference)
      )
    })
  })
}
