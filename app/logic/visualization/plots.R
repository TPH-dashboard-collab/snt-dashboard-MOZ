box::use(
  checkmate[qtest],
  ggplot2,
  rlang[sym],
  scales,
)

# fmt: skip
box::use(
  app/logic/visualization/color_palettes[create_blue_palette],
  app/logic/visualization/plotly_custom[build_interactive_plot],
  app/logic/visualization/tables[format_with_ci],
)

# Prevalence/Incidence ---------------------------------------------------------

#' Generate interactive yearly metric plot with scenario comparison
#' @param data A data.table containing aggregated results. Must include columns:
#'   * year: integer
#'   * scenario_name: character
#'   * metric_mean: numeric
#'   * metric_lci: numeric
#'   * metric_uci: numeric
#' @param scenarios Character vector of scenario names to include in
#'   visualization
#' @param reference Character string or a reactive expression returning a
#'   character string specifying the baseline scenario
#' @param scenario_mapping Named character vector mapping scenario IDs to
#'   display names
#' @param opts List of optional parameters:
#'   * y_label: Custom label for y-axis
#'   * colors: Custom color vector for scenarios
#' @export
yearlyMetric_plot <- function(
  data,
  opts,
  scenario_mapping,
  scenarios,
  reference
) {
  y_label <- if (is.null(opts[["y_label"]])) {
    "Metric"
  } else {
    opts[["y_label"]]
  }

  # Add human-readable scenario names
  scenario_mapping[reference] <- "Reference"
  data[, pretty_name := scenario_mapping[scenario_name]]

  # Create a color palette for scenarios
  if (is.null(opts[["colors"]])) {
    n_scenarios <- length(scenarios)
    n_reference <- length(reference)
    colors <- create_blue_palette(n_scenarios + n_reference)
  } else {
    colors <- opts[["colors"]]
  }

  # Create the ggplot object
  g <- ggplot2$ggplot(
    data,
    ggplot2$aes(
      x = year,
      y = metric_mean,
      color = pretty_name,
      group = scenario_name
    )
  ) +
    # Add points with custom hover text
    ggplot2$geom_point(
      ggplot2$aes(
        text = paste0(
          "</br> Scenario: ",
          pretty_name,
          "</br> Year: ",
          year,
          "</br>",
          y_label,
          ": ",
          format(
            metric_mean,
            trim = TRUE,
            digits = 1
          ),
          " [",
          format(metric_lci, trim = TRUE, digits = 1),
          " : ",
          format(metric_uci, trim = TRUE, digits = 1),
          "]"
        )
      ),
      position = ggplot2$position_dodge(width = 0.2)
    ) +
    # Add error bars
    ggplot2$geom_errorbar(
      ggplot2$aes(ymin = metric_lci, ymax = metric_uci),
      position = ggplot2$position_dodge(width = 0.2)
    ) +
    # Add smoothed line
    ggplot2$geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
    # Set color scale
    ggplot2$scale_color_manual(
      name = "Scenario",
      values = colors
    ) +
    # Set labels
    ggplot2$labs(
      title = paste("Yearly", y_label, "by Scenario"),
      x = "Year",
      y = y_label
    ) +
    # Set theme
    ggplot2$theme(legend.position = "bottom") +
    ggplot2$theme_minimal()

  # Convert to interactive Plotly chart
  build_interactive_plot(g, data, csv_name = "yearly_metric_data.csv")
}

#' Generate interactive yearly metric reduction plot with scenario comparison
#' @param data A data.table containing aggregated results. Must include columns:
#'   * year: integer
#'   * scenario_name: character
#'   * relative_reduction_mean: numeric
#'   * relative_reduction_lci: numeric
#'   * relative_reduction_uci: numeric
#' @param reference Character string or a reactive expression returning a
#'   character string specifying the baseline scenario
#' @param scenario_mapping Named character vector mapping scenario IDs to
#'   display names
#' @param opts List of optional parameters:
#'   * colors: Custom color vector for scenarios
#'   * title: Custom plot title
#' @export
yearlyMetricReduction_plot <- function(
  data,
  reference,
  scenario_mapping,
  opts
) {
  # Input validation
  stopifnot(
    "data must be a non-empty data frame" = qtest(data, "D+") && nrow(data) > 0,
    "reference must be a string" = qtest(reference, "S1"),
    "scenario_mapping must be a named character vector" = qtest(
      scenario_mapping,
      "S+"
    ) &&
      !is.null(names(scenario_mapping)),
    "opts must be a list" = qtest(opts, "L")
  )

  # Add human-readable scenario names
  data[, pretty_name := scenario_mapping[scenario_name]]

  # Create color palette for scenarios
  if (is.null(opts[["colors"]])) {
    colors <- create_blue_palette(length(unique(data[, scenario_name])))
  } else {
    colors <- opts[["colors"]]
  }

  # Create the ggplot object
  g <- ggplot2$ggplot(
    # Exclude the reference scenario
    data[scenario_name != reference],
    ggplot2$aes(
      x = factor(year),
      y = relative_reduction_mean,
      fill = pretty_name,
      # Custom tooltip text
      text = paste0(
        "</br> Scenario: ",
        pretty_name,
        "</br> Year: ",
        year,
        "</br> Reduction: ",
        format(
          round(relative_reduction_mean * 100, digits = 0),
          trim = TRUE
        ),
        "%",
        "</br> Range: [",
        format(
          round(relative_reduction_lci * 100, digits = 1),
          nsmall = 1
        ),
        "%, ",
        format(
          round(relative_reduction_uci * 100, digits = 1),
          nsmall = 1
        ),
        "%]"
      )
    )
  ) +
    # Add bars representing mean reduction
    ggplot2$geom_bar(
      stat = "identity",
      position = ggplot2$position_dodge(width = 0.9)
    ) +
    # Add error bars for confidence intervals
    ggplot2$geom_errorbar(
      ggplot2$aes(ymin = relative_reduction_lci, ymax = relative_reduction_uci),
      position = ggplot2$position_dodge(width = 0.9),
      width = 0.25
    ) +
    # Set y-axis to percentage scale, with a slight extension above 100%
    # for visibility
    ggplot2$scale_y_continuous(labels = scales$percent_format()) +
    # Apply the custom color palette
    ggplot2$scale_fill_manual(values = colors) +
    # Set plot labels
    ggplot2$labs(
      title = if (is.null(opts[["title"]])) {
        "Relative Reduction by Year and Scenario"
      } else {
        opts[["title"]]
      },
      x = "Year",
      y = "Relative Reduction",
      fill = "Scenario"
    ) +
    # Apply a minimal theme
    ggplot2$theme(
      legend.position = "bottom",
      # Angle x-axis labels for readability
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1)
    ) +
    ggplot2$theme_minimal()

  # Convert to interactive Plotly chart
  build_interactive_plot(g, data, csv_name = "yearly_reduction_data.csv")
}

#' Generate interactive bar chart of total costs by scenario
#'
#' Creates a bar chart showing total intervention costs (in millions USD) for
#' each scenario, with error bars for confidence intervals. Converted to an
#' interactive Plotly chart with hover tooltips.
#'
#' @param costs_dt A data.table containing cost data. Must include columns:
#'   * scenario_name: character
#'   * pretty_name: character (display name for scenario)
#'   * total_cost_mean: numeric
#'   * total_cost_lci: numeric
#'   * total_cost_uci: numeric
#' @return A plotly object
#' @export
costs_plot <- function(costs_dt) {
  # Input validation
  stopifnot(
    "costs_dt must be a non-empty data frame" = qtest(costs_dt, "D+") &&
      nrow(costs_dt) > 0
  )

  # Create color palette for scenarios
  colors <- create_blue_palette(
    length(unique(costs_dt[, scenario_name]))
  )

  # Build ggplot visualization
  g <- ggplot2$ggplot(
    data = costs_dt,
    ggplot2$aes(
      x = pretty_name,
      y = total_cost_mean / 1000000,
      fill = pretty_name,
      text = paste0(
        "</br> Scenario: ",
        pretty_name,
        "</br> Total Cost: ",
        format_with_ci(
          total_cost_mean,
          total_cost_lci,
          total_cost_uci
        )
      )
    )
  ) +
    # Add bars for mean costs
    ggplot2$geom_bar(
      stat = "identity",
      position = ggplot2$position_dodge(width = 0.9)
    ) +
    # Add error bars for confidence intervals
    ggplot2$geom_errorbar(ggplot2$aes(
      ymin = total_cost_lci / 1000000,
      ymax = total_cost_lci / 1000000,
      width = 0.25
    )) +
    # Set color scale
    ggplot2$scale_fill_manual(values = colors) +
    # Set axis labels
    ggplot2$labs(
      x = NULL,
      y = "Costs in Mio. USD"
    ) +
    # Apply minimal theme
    ggplot2$theme_minimal() +
    # Remove legend
    ggplot2$theme(legend.position = "none")

  # Convert to interactive Plotly chart
  build_interactive_plot(g, data = costs_dt, csv_name = "cost_data.csv")
}

#' Generate interactive bar chart of cost per case averted by scenario
#'
#' Creates a bar chart showing the cost per averted case (in USD) for each
#' scenario, with error bars for confidence intervals. Converted to an
#' interactive Plotly chart with hover tooltips.
#'
#' @param plot_dt A data.table containing cost-effectiveness data. Must include
#'   columns:
#'   * scenario_name: character
#'   * pretty_name: character (display name for scenario)
#'   * cost_per_averted_mean: numeric
#'   * cost_per_averted_lci: numeric
#'   * cost_per_averted_uci: numeric
#' @return A plotly object
#' @export
costs_averted_plot <- function(plot_dt) {
  # Input validation
  stopifnot(
    "plot_dt must be a non-empty data frame" = qtest(plot_dt, "D+") &&
      nrow(plot_dt) > 0
  )

  # Create color palette for scenarios
  colors <- create_blue_palette(length(unique(plot_dt[, scenario_name])))

  # Build ggplot visualization
  g <- ggplot2$ggplot(
    data = plot_dt,
    ggplot2$aes(
      x = pretty_name,
      y = cost_per_averted_mean,
      fill = pretty_name,
      text = paste0(
        "</br> Scenario: ",
        pretty_name,
        "</br> Cost/Case Averted: ",
        format(
          round(cost_per_averted_mean),
          big.mark = " "
        ),
        "</br> Range: [",
        format(
          round(
            cost_per_averted_lci
          ),
          big.mark = " "
        ),
        " : ",
        format(
          round(
            cost_per_averted_uci
          ),
          big.mark = " "
        ),
        "]"
      )
    )
  ) +
    # Add bars for mean cost-effectiveness
    ggplot2$geom_bar(
      stat = "identity",
      position = ggplot2$position_dodge(width = 0.9)
    ) +
    # Add error bars for confidence intervals
    ggplot2$geom_errorbar(ggplot2$aes(
      ymin = cost_per_averted_lci,
      ymax = cost_per_averted_uci,
      width = 0.25
    )) +
    # Set color scale
    ggplot2$scale_fill_manual(values = colors) +
    # Set axis labels
    ggplot2$labs(
      x = NULL,
      y = "Costs in USD"
    ) +
    # Apply minimal theme
    ggplot2$theme_minimal() +
    # Remove legend
    ggplot2$theme(legend.position = "none")

  # Convert to interactive Plotly chart
  build_interactive_plot(g, data = plot_dt, csv_name = "cost_averted_data.csv")
}

#' @export
relativeImpact_plot <- function(data, metric, opts = list()) {
  # Dynamically define metric columns
  mean_col <- sym(paste0(metric, "_mean"))
  lci_col <- sym(paste0(metric, "_lci"))
  uci_col <- sym(paste0(metric, "_uci"))

  # Define color palette
  if (is.null(opts[["colors"]])) {
    colors <- create_blue_palette(length(unique(data[, scenario_name])))
  } else {
    colors <- opts[["colors"]]
  }

  # Create the ggplot object
  g <- ggplot2$ggplot(
    data,
    ggplot2$aes(
      x = pretty_name,
      y = !!mean_col,
      fill = pretty_name,
      text = paste0(
        "</br> Scenario: ",
        pretty_name,
        "</br> Cases Averted: ",
        format(round(!!mean_col), big.mark = " "),
        "</br> Range: [",
        format(round(!!lci_col), big.mark = " "),
        " : ",
        format(round(!!uci_col), big.mark = " "),
        "]"
      )
    )
  ) +
    # Add bars for mean impact
    ggplot2$geom_bar(
      stat = "identity",
      position = ggplot2$position_dodge(width = 0.9)
    ) +
    # Add error bars for confidence intervals
    ggplot2$geom_errorbar(
      ggplot2$aes(ymin = !!lci_col, ymax = !!uci_col, width = 0.25)
    ) +
    # Set color scale
    ggplot2$scale_fill_manual(values = colors) +
    # Set labels
    ggplot2$labs(
      x = NULL,
      y = "Cases Averted"
    ) +
    # Set theme
    ggplot2$theme_minimal() +
    ggplot2$theme(legend.position = "none")

  # Convert to interactive Plotly chart
  build_interactive_plot(g, data, csv_name = "relative_impact_data.csv")
}
