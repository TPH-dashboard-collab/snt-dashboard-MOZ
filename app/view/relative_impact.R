box::use(
  config,
  logger[log_debug],
  plotly[plotlyOutput, renderPlotly],
  shiny,
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/visualization/plots[relativeImpact_plot],
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
  plotlyOutput(ns("plot"))
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
#' @return A Shiny server function that handles map rendering and updates
#' @export
server <- function(
  id,
  variables,
  use_custom_data = FALSE,
  year_start,
  year_end,
  metric,
  trigger,
  opts,
  ...
) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "relative_impact")

      # Reactive for data preparation
      plot_data <- shiny$reactive({
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

        # Select relevant columns
        cols <- c(
          "scenario_name",
          paste0(metric, c("_mean", "_lci", "_uci"))
        )
        dt <- dt[, ..cols]

        # REVIEW 2025-01-20: This is an approximation at best, I think. We
        #   assume that the CI interval is constant and calculate the
        #   relative difference between mean and UCI/LCI. Then we
        #   calculate the difference between the reference and the
        #   scenarios and apply the same relative difference again to get
        #   the LCI and UCI. See also `calculate_relative_reduction.R`.

        # Calculate relative difference of LCI/UCI to the mean
        rel_lci <- paste0(metric, "_rel_lci")
        rel_uci <- paste0(metric, "_rel_uci")
        dt <- dt[,
          (rel_lci) := (mean_col - lci_col) / mean_col,
          env = list(
            mean_col = paste0(metric, "_mean"),
            lci_col = paste0(metric, "_lci")
          )
        ]
        dt <- dt[,
          (rel_uci) := (uci_col - mean_col) / mean_col,
          env = list(
            mean_col = paste0(metric, "_mean"),
            uci_col = paste0(metric, "_uci")
          )
        ]

        # Calculate relative impact (difference from reference)
        # The negative sign is used to represent cases averted (positive
        # values)
        dt <- dt[,
          c(2:(ncol(dt) - 2)) := lapply(
            .SD,
            function(x) {
              -1 *
                (x -
                  x[
                    scenario_name == variables$session_state$reference
                  ])
            }
          ),
          .SDcols = c(2:(ncol(dt) - 2))
        ]

        dt <- dt[,
          lci_col := mean_col - (mean_col * rel_lci_col),
          env = list(
            lci_col = paste0(metric, "_lci"),
            mean_col = paste0(metric, "_mean"),
            rel_lci_col = rel_lci
          )
        ]
        dt <- dt[,
          uci_col := mean_col + (mean_col * rel_uci_col),
          env = list(
            uci_col = paste0(metric, "_uci"),
            mean_col = paste0(metric, "_mean"),
            rel_uci_col = rel_uci
          )
        ]

        # Remove BAU (Business As Usual) scenarios
        dt <- dt[scenario_name != variables$session_state$reference]

        # Add human-readable scenario names
        dt[, pretty_name := variables$scenario_mapping[scenario_name]]

        log_debug(
          "Aggregating data ... done",
          namespace = log_ns_fn(log_ns)
        )

        dt
      })

      output$plot <- renderPlotly({
        log_debug(
          "Rendering relative impact plot ...",
          namespace = log_ns_fn(log_ns)
        )
        data <- plot_data()

        relativeImpact_plot(
          data = data,
          metric = metric,
          opts = opts
        )
      })
    }
  )
}
