box::use(
  checkmate[qtest],
  data.table[`%chin%`, rbindlist, setcolorder, setDT, transpose],
  stats[setNames],
)

# fmt: skip
box::use(
  app/logic/analytics/costs[calculate_cost_effectiveness],
)

#' Format impact metrics into a presentation table
#'
#' This function takes aggregated impact metrics and formats them into a table
#' with the desired presentation format of "mean [min:max]" for each metric.
#'
#' @param impact_data A data.table containing aggregated impact metrics from
#'   aggregate_impact() function
#' @param reference Character string specifying the reference scenario
#'   name
#' @param scenarios Character vector of scenario names to include
#' @param digits Number of digits for rounding. Default is 0.
#' @param impact_mapping Mapping of impact metric and pretty names.
#' @param scenario_mapping Named character vector mapping scenario IDs to
#'   display names
#'
#' @return A data.table formatted for presentation with metrics as rows and
#'   scenarios as columns
#'
#' @examplesIf interactive()
#' formatted_table <- format_impact_table(
#'   impact_data = aggregated_impacts,
#'   reference = "BAU_country_vaccine_0",
#'   scenarios = c("NSP_country_vaccine_0", "NSP_country_vaccine_0.6")
#' )
#' @export
format_impact_table <- function(
  impact_data,
  reference,
  scenarios,
  digits = 0,
  impact_mapping,
  scenario_mapping
) {
  # Input validation
  stopifnot(
    "impact_data must be a non-empty data frame" = qtest(impact_data, "D+") &&
      nrow(impact_data) > 0,
    "reference must be a string" = qtest(reference, "S1"),
    "scenarios must be a character vector" = qtest(scenarios, "S+"),
    "digits must be a single integer" = qtest(digits, "X1"),
    "impact_mapping must be a named character vector" = qtest(
      impact_mapping,
      "S+"
    ) &&
      !is.null(names(impact_mapping)),
    "scenario_mapping must be a named character vector" = qtest(
      scenario_mapping,
      "S+"
    ) &&
      !is.null(names(scenario_mapping))
  )

  # Ensure input is a data.table
  setDT(impact_data)
  impact_data <- rbindlist(
    l = list(
      impact_data[scenario_name %chin% scenarios][,
        scenario_name := scenario_mapping[scenario_name]
      ],
      impact_data[scenario_name %chin% reference][,
        scenario_name := "Reference"
      ]
    )
  )

  impact_data <- impact_data[,
    c(2:ncol(impact_data)) := lapply(
      .SD,
      function(x) -1 * (x - x[scenario_name == "Reference"])
    ),
    .SDcols = c(2:ncol(impact_data))
  ]

  # Define the metrics we want to show
  metrics <- impact_mapping

  to_bind <- list()
  for (scen in unique(impact_data[, scenario_name])) {
    row <- list()
    row[["scenario_name"]] <- scen

    for (metric_col in names(metrics)) {
      mean_col <- paste0(metric_col, "_mean")
      min_col <- paste0(metric_col, "_lci")
      max_col <- paste0(metric_col, "_uci")

      row[[metric_col]] <- format_with_ci(
        impact_data[scenario_name == scen, ..mean_col],
        impact_data[scenario_name == scen, ..min_col],
        impact_data[scenario_name == scen, ..max_col]
      )
    }
    to_bind <- c(to_bind, list(row))
  }

  result <- rbindlist(l = to_bind)

  result <- transpose(
    result,
    keep.names = "Metric",
    make.names = "scenario_name"
  )
  result <- result[, Metric := metrics[Metric]][, Reference := NULL]
  setcolorder(result, c("Metric"))
  return(result)
}

#' Function to format a single value with confidence intervals.
#'
#' This function will produce a string in the format of "mean [min:max]".
#'
#' @param mean_val Mean value.
#' @param min_val Min value.
#' @param max_val Max value.
#' @param digits Number of digits for rounding. Default is 0.
#'
#' @return A string in the format of "mean [min:max]".
#' @export
format_with_ci <- function(mean_val, min_val, max_val, digits = 0) {
  # Input validation: accept numeric vectors or single-column data.tables
  # (data.table's ..col syntax returns a 1-column data.table)
  is_numeric_like <- function(x) {
    qtest(x, "N+") || (is.data.frame(x) && ncol(x) == 1 && is.numeric(x[[1]]))
  }
  stopifnot(
    "mean_val must be numeric" = is_numeric_like(mean_val),
    "min_val must be numeric" = is_numeric_like(min_val),
    "max_val must be numeric" = is_numeric_like(max_val),
    "digits must be a single integer" = qtest(digits, "X1")
  )

  sprintf(
    "%s<br>[%s : %s]",
    format(round(mean_val, digits), big.mark = " ", trim = TRUE),
    format(round(min_val, digits), big.mark = " ", trim = TRUE),
    format(round(max_val, digits), big.mark = " ", trim = TRUE)
  )
}

#' Format Cost Analysis Results into a Presentable Table
#'
#' Creates a formatted table comparing costs and cost-effectiveness metrics
#' across different scenarios and a reference. The table includes total
#' costs and cumulative costs per cases averted, with confidence intervals.
#'
#' @param cost_data A data.table containing cost information for different
#'   scenarios from `calculate_intervention_costs()`
#' @param sim_data A data.table containing simulation results. Must include
#'   columns:
#'   * scenario_name: character,
#'   * admin_1: character
#'   * EIR_CI: numeric
#'   * cum_nUncomp: numeric
#'   * cum_nSevere: numeric
#'   * cum_tUncomp: numeric
#'   * cum_tSevere: numeric
#'   * cum_expectedDirectDeaths: numeric
#' @param reference Character vector specifying the reference scenario
#'   name
#' @param scenarios Character vector of scenario names to include in comparison
#' @param digits Integer indicating number of decimal places for rounding
#'   (default: 0)
#' @param scenario_mapping Named character vector mapping scenario IDs to
#'   display names
#' @return A data.table with formatted cost metrics as rows and scenarios as
#'   columns, including confidence intervals in parentheses where applicable
#' @export
format_costs_table <- function(
  cost_data,
  sim_data,
  reference,
  scenarios,
  digits = 0,
  scenario_mapping
) {
  # Input validation
  stopifnot(
    "cost_data must be a non-empty data frame" = qtest(cost_data, "D+") &&
      nrow(cost_data) > 0,
    "sim_data must be a non-empty data frame" = qtest(sim_data, "D+") &&
      nrow(sim_data) > 0,
    "reference must be a string" = qtest(reference, "S1"),
    "scenarios must be a character vector" = qtest(scenarios, "S+"),
    "digits must be a single integer" = qtest(digits, "X1"),
    "scenario_mapping must be a named character vector" = qtest(
      scenario_mapping,
      "S+"
    ) &&
      !is.null(names(scenario_mapping))
  )

  # Ensure input is a data.table
  setDT(cost_data)

  averted_cost_data <- calculate_cost_effectiveness(
    cost_data,
    sim_data[scenario_name %chin% c(scenarios, reference)],
    reference
  )
  averted_cost_data <- averted_cost_data[scenario_name %chin% scenarios][,
    scenario_name := scenario_mapping[scenario_name]
  ]

  cost_data <- rbindlist(
    l = list(
      cost_data[scenario_name %chin% scenarios][,
        scenario_name := scenario_mapping[scenario_name]
      ],
      cost_data[scenario_name %chin% reference][,
        scenario_name := "Reference"
      ]
    )
  )

  # Extract individual intervention cost columns from cost_data

  # Pattern: {intervention}_cost_mean columns
  int_cost_cols <- grep("_cost_mean$", names(cost_data), value = TRUE)
  int_cost_cols <- int_cost_cols[int_cost_cols != "total_cost_mean"]
  # Extract intervention names (remove _cost_mean suffix)
  int_names <- gsub("_cost_mean$", "", int_cost_cols)

  # Build metrics: intervention costs + total cost + cost per averted
  metrics <- c(
    setNames(
      paste0(toupper(int_names), " costs"),
      paste0(int_names, "_cost")
    ),
    "total_cost" = "Total costs",
    "cost_per_averted" = "Cum. costs per cases averted"
  )

  # Get all cost metrics (intervention + total)
  cost_metrics <- names(metrics)[names(metrics) != "cost_per_averted"]

  to_bind <- list()
  for (scen in unique(cost_data[, scenario_name])) {
    row <- list()
    row[["scenario_name"]] <- scen

    for (metric_col in cost_metrics) {
      mean_col <- paste0(metric_col, "_mean")
      min_col <- paste0(metric_col, "_lci")
      max_col <- paste0(metric_col, "_uci")

      row[[metric_col]] <- format_with_ci(
        cost_data[scenario_name == scen, ..mean_col],
        cost_data[scenario_name == scen, ..min_col],
        cost_data[scenario_name == scen, ..max_col]
      )
    }
    to_bind <- c(to_bind, list(row))
  }
  costs_dt <- rbindlist(l = to_bind)

  to_bind <- list()
  for (scen in unique(averted_cost_data[, scenario_name])) {
    row <- list()
    row[["scenario_name"]] <- scen

    row[["cost_per_averted"]] <- format_with_ci(
      averted_cost_data[scenario_name == scen, cost_per_averted_mean],
      averted_cost_data[scenario_name == scen, cost_per_averted_lci],
      averted_cost_data[scenario_name == scen, cost_per_averted_uci]
    )
    to_bind <- c(to_bind, list(row))
  }
  costs_averted_dt <- rbindlist(l = to_bind)

  result <- costs_averted_dt[costs_dt, on = "scenario_name"]

  result <- transpose(
    result,
    keep.names = "Metric",
    make.names = "scenario_name"
  )
  result <- result[, Metric := metrics[Metric]]
  setcolorder(result, c("Metric", "Reference"))
  result <- result[, lapply(.SD, function(x) {
    ifelse(is.na(x), "--", x)
  })]
  return(result)
}
