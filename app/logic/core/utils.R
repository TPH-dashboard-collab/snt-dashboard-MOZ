box::use(
  checkmate[qtest],
  data.table[`%chin%`, dcast, setDT, setnames],
)

#' Get the value from a named vector by name
#'
#' The function returns the first match.
#'
#' @param x A named vector.
#' @param name The name of the value to retrieve.
#' @return The value associated with the given name.
#' @export
nvec_get_val <- function(x, name) {
  stopifnot("x must be a non-empty vector" = qtest(x, "V+"))

  return(x[[name]])
}

#' Get the name from a named vector by value
#'
#' The function returns the first match.
#'
#' @param x A named vector.
#' @param val The value to search for.
#' @return The name associated with the given value.
#' @export
nvec_get_name <- function(x, val) {
  stopifnot("x must be a non-empty vector" = qtest(x, "V+"))

  return(names(x[x == val])[1])
}

#' Aggregate a metric (e,g. incidence) across seeds and EIR values
#'
#' This function aggregates a metric like incidence or prevalence data from
#' simulation results, accounting for different seeds and EIR confidence
#' intervals.
#'
#' @param dt A data.table containing simulation results. Must include columns:
#'   * year: integer
#'   * scenario_name: character
#'   * seed: integer
#'   * EIR_CI: numeric
#'   * nHost: numeric
#'   * the column specified in the metric parameter: numeric
#' @param metric A character string specifying the column name of the metric to
#'   aggregate.
#'
#' @return A data.table with aggregated metrics, including lower and upper
#'   confidence intervals.
#'
#' @examplesIf interactive()
#' # Assuming 'sim_results' is your data.table with simulation results
#' agg_incidence <- aggregate_metric(sim_results, "incidenceRate")
#' agg_prevalence <- aggregate_metric(sim_results, "prevalenceRate")
#' @export
aggregate_metric <- function(dt, metric) {
  # Input validation
  stopifnot(
    "dt must be a non-empty data frame" = qtest(dt, "D+") && nrow(dt) > 0,
    "metric must be a string" = qtest(metric, "S1")
  )

  # Ensure input is a data.table
  setDT(dt)

  # Check if the specified metric is present in the input data.table
  if (!metric %chin% colnames(dt)) {
    stop(paste0("Metric '", metric, "' not found in input."))
  }

  # Calculate weighted average of the specified metric, weighted by number of
  # hosts
  agg_by_eir <- dt[,
    .(
      metric_value = sum(get(metric) * nHost) / sum(nHost)
    ),
    by = .(year, scenario_name, seed, EIR_CI)
  ]

  # Calculate the mean of the metric value by year, scenario_name, and EIR_CI
  agg_by_eir <- agg_by_eir[,
    .(metric_value = mean(metric_value)),
    by = .(year, scenario_name, EIR_CI)
  ]

  # Reshape data from long to wide format
  agg_data <- dcast(
    agg_by_eir,
    year + scenario_name ~ EIR_CI,
    value.var = "metric_value"
  )

  # Rename columns to reflect their content
  setnames(
    agg_data,
    old = c("EIR_lci", "EIR_mean", "EIR_uci"),
    new = c("metric_lci", "metric_mean", "metric_uci")
  )

  # Return the aggregated data
  return(agg_data)
}

#' Calculate relative reduction in metrics compared to a reference
#'
#' This function computes the relative reduction in a given metric (e.g.,
#' incidence or prevalence) for different scenarios compared to a specified
#' reference scenario.
#'
#' @param dt A data.table containing aggregated metric data (see
#'   `aggregate_metric`). Must include columns:
#'   * year
#'   * scenario_name
#'   * metric_lci
#'   * metric_mean
#'   * metric_uci
#' @param reference A string specifying the name of the reference
#'   scenario
#'
#' @return A data.table with relative reductions for each scenario and year,
#'   including lower and upper confidence intervals
#'
#' @examplesIf interactive()
#' # Assuming 'agg_data' is your aggregated data.table
#' relative_reductions <- calculate_relative_reduction(agg_data,
#'   reference = "baseline"
#' )
#' @export
calculate_relative_reduction <- function(dt, reference) {
  # Input validation
  stopifnot(
    "dt must be a non-empty data frame" = qtest(dt, "D+") && nrow(dt) > 0,
    "reference must be a string" = qtest(reference, "S1"),
    "dt must contain required columns" =
      all(c("year", "scenario_name", "metric_lci", "metric_mean", "metric_uci")
        %in% names(dt)),
    "reference must be present in dt$scenario_name" =
      reference %in% dt$scenario_name
  )

  # Ensure input is data.table
  setDT(dt)

  # REVIEW 2025-01-20: This is an approximation at best, I think. We assume that
  #   the CI interval is constant and calculate the relative difference between
  #   mean and UCI/LCI. Then we calculate the difference between the
  #   reference and the scenarios and apply the same relative difference
  #   again to get the LCI and UCI. See also `mod-cum_relative_impact_metric.R`.

  # Calculate relative difference of LCI/UCI to the mean
  rel_lci <- "metric_rel_lci"
  rel_uci <- "metric_rel_uci"
  dt <- dt[,
    (rel_lci) := (mean_col - lci_col) / mean_col,
    env = list(
      mean_col = "metric_mean",
      lci_col = "metric_lci"
    )
  ]
  dt <- dt[,
    (rel_uci) := (uci_col - mean_col) / mean_col,
    env = list(
      mean_col = "metric_mean",
      uci_col = "metric_uci"
    )
  ]

  dt <- dt[,
    metric_mean := lapply(.SD, function(x) {
      (x[scenario_name == reference] - x) /
        x[scenario_name == reference]
    }),
    .SDcols = c("metric_mean"),
    by = c("year")
  ]

  dt <- dt[,
    lci_col := mean_col - (mean_col * rel_lci_col),
    env = list(
      lci_col = "metric_lci",
      mean_col = "metric_mean",
      rel_lci_col = rel_lci
    )
  ]
  dt <- dt[,
    uci_col := mean_col + (mean_col * rel_uci_col),
    env = list(
      uci_col = "metric_uci",
      mean_col = "metric_mean",
      rel_uci_col = rel_uci
    )
  ]
  dt[, c("metric_rel_lci", "metric_rel_uci") := NULL]

  # Rename columns to clearly indicate they represent relative reductions
  setnames(
    dt,
    old = c("metric_lci", "metric_mean", "metric_uci"),
    new = c(
      "relative_reduction_lci",
      "relative_reduction_mean",
      "relative_reduction_uci"
    )
  )

  return(dt)
}

#' Aggregate impact metrics across scenarios and EIR confidence intervals
#'
#' This function aggregates various impact metrics (e.g., uncomplicated cases,
#' severe cases, deaths) for different scenarios and Entomological Inoculation
#' Rate (EIR) confidence intervals.
#'
#' For year ranges, it calculates the increment during the selected period by
#' subtracting cumulative values at (year_start - 1) from values at year_end.
#'
#' @param data A data.table containing simulation results. Must include columns:
#'   * scenario_name: character,
#'   * admin_1: character
#'   * EIR_CI: numeric
#'   * cum_nUncomp: numeric
#'   * cum_nSevere: numeric
#'   * cum_tUncomp: numeric
#'   * cum_tSevere: numeric
#'   * cum_expectedDirectDeaths: numeric
#' @param year_start The specific start year for which to aggregate the data
#' @param year_end The specific end year for which to aggregate the data
#'
#' @return A data.table with aggregated impact metrics for each scenario and EIR
#'   confidence interval
#'
#' @examplesIf interactive()
#' # Assuming 'sim_results' is your data.table with simulation results
#' aggregated_impact <- aggregate_impact(sim_results, year_start = 2025, year_end = 2030)
#' @export
aggregate_impact <- function(data, year_start = 2026, year_end = 2026) {
  # Input validation
  stopifnot(
    "data must be a non-empty data frame" = qtest(data, "D+") && nrow(data) > 0,
    "year_start must be a single integer" = qtest(year_start, "X1"),
    "year_end must be a single integer" = qtest(year_end, "X1")
  )

  # Ensure input is a data.table
  setDT(data)

  # Validate year parameters
  stopifnot(
    "year_start must be <= year_end" = year_start <= year_end
  )

  # Get year range in data for validation and edge case handling
  min_year <- min(data$year)
  max_year <- max(data$year)

  # Validate year_end exists in data
  if (year_end > max_year || year_end < min_year) {
    stop(
      "year_end (", year_end, ") is outside available data range (",
      min_year, "-", max_year, ")"
    )
  }

  # Warn if year_start is before available data (will use earliest available)
  if (year_start < min_year) {
    warning(
      "year_start (", year_start, ") is before earliest data year (",
      min_year, "). Using cumulative values at year_end."
    )
  }

  if (year_start == year_end) {
    # Single year: filter to that year
    data <- data[year == year_start]

    # Calculate means across seeds
    data <- data[,
      .(
        cum_nUncomp = mean(cum_nUncomp),
        cum_nSevere = mean(cum_nSevere),
        cum_tUncomp = mean(cum_tUncomp),
        cum_tSevere = mean(cum_tSevere),
        cum_expectedDirectDeaths = mean(cum_expectedDirectDeaths)
      ),
      by = .(scenario_name, admin_1, admin_2, EIR_CI)
    ]
  } else {
    # Year range: calculate increment (end - start_minus_1)
    # Get data at year_end
    data_end <- data[year == year_end]
    data_end <- data_end[,
      .(
        cum_nUncomp_end = mean(cum_nUncomp),
        cum_nSevere_end = mean(cum_nSevere),
        cum_tUncomp_end = mean(cum_tUncomp),
        cum_tSevere_end = mean(cum_tSevere),
        cum_expectedDirectDeaths_end = mean(cum_expectedDirectDeaths)
      ),
      by = .(scenario_name, admin_1, admin_2, EIR_CI)
    ]

    # Get data at year_start - 1 (baseline)
    baseline_year <- year_start - 1
    if (baseline_year >= min_year) {
      data_start <- data[year == baseline_year]
      data_start <- data_start[,
        .(
          cum_nUncomp_start = mean(cum_nUncomp),
          cum_nSevere_start = mean(cum_nSevere),
          cum_tUncomp_start = mean(cum_tUncomp),
          cum_tSevere_start = mean(cum_tSevere),
          cum_expectedDirectDeaths_start = mean(cum_expectedDirectDeaths)
        ),
        by = .(scenario_name, admin_1, admin_2, EIR_CI)
      ]

      # Merge and calculate difference
      data <- merge(
        data_end,
        data_start,
        by = c("scenario_name", "admin_1", "admin_2", "EIR_CI")
      )
      data <- data[,
        .(
          cum_nUncomp = cum_nUncomp_end - cum_nUncomp_start,
          cum_nSevere = cum_nSevere_end - cum_nSevere_start,
          cum_tUncomp = cum_tUncomp_end - cum_tUncomp_start,
          cum_tSevere = cum_tSevere_end - cum_tSevere_start,
          cum_expectedDirectDeaths = cum_expectedDirectDeaths_end -
            cum_expectedDirectDeaths_start
        ),
        by = .(scenario_name, admin_1, admin_2, EIR_CI)
      ]
    } else {
      # No baseline available, use year_end values directly
      setnames(
        data_end,
        c(
          "cum_nUncomp_end",
          "cum_nSevere_end",
          "cum_tUncomp_end",
          "cum_tSevere_end",
          "cum_expectedDirectDeaths_end"
        ),
        c(
          "cum_nUncomp",
          "cum_nSevere",
          "cum_tUncomp",
          "cum_tSevere",
          "cum_expectedDirectDeaths"
        )
      )
      data <- data_end
    }
  }

  # Sum up across all admin regions
  data <- data[,
    .(
      cum_nUncomp = sum(cum_nUncomp),
      cum_nSevere = sum(cum_nSevere),
      cum_tUncomp = sum(cum_tUncomp),
      cum_tSevere = sum(cum_tSevere),
      cum_expectedDirectDeaths = sum(cum_expectedDirectDeaths)
    ),
    by = .(scenario_name, EIR_CI)
  ]

  # Reshape to wide format
  data <- dcast(
    data,
    scenario_name ~ EIR_CI,
    value.var = c(
      "cum_nUncomp",
      "cum_nSevere",
      "cum_tUncomp",
      "cum_tSevere",
      "cum_expectedDirectDeaths"
    )
  )

  idx <- 2:ncol(data)

  setnames(
    data,
    old = idx,
    new = gsub("_EIR_", "_", names(data)[idx])
  )

  return(data)
}
