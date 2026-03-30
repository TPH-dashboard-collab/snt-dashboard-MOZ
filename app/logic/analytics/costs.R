box::use(
  checkmate[qtest],
  data.table[`%chin%`, dcast, patterns, setDT, setnames],
)

# fmt: skip
box::use(
  app/logic/core/utils[aggregate_impact],
)

#' Calculate cost per case or death averted
#'
#' This function calculates the cost-effectiveness of interventions by comparing
#' the total intervention costs against the number of uncomplicated cases
#' averted relative to a reference scenario.
#'
#' @param costs_dt Output from `calculate_intervention_costs()`. Must include
#'   columns:
#'   * scenario_name: character
#'   * total_cost_lci: numeric (lower confidence interval of total cost)
#'   * total_cost_mean: numeric (mean total cost)
#'   * total_cost_uci: numeric (upper confidence interval of total cost)
#' @param sim_dt A data.table containing simulation results. Must include
#'   columns:
#'   * scenario_name: character
#'   * admin_1: character
#'   * admin_2: character
#'   * EIR_CI: numeric
#'   * year: integer
#'   * cum_nUncomp: numeric (cumulative uncomplicated cases)
#'   * cum_nSevere: numeric (cumulative severe cases)
#'   * cum_tUncomp: numeric (cumulative treatment of uncomplicated cases)
#'   * cum_tSevere: numeric (cumulative treatment of severe cases)
#'   * cum_expectedDirectDeaths: numeric (cumulative expected direct deaths)
#' @param reference Reference scenario name (character)
#' @param year_start The specific start year for which to aggregate the data (integer)
#' @param year_end: The specific end year for which to aggregate the data (integer)
#'
#' @return data.table with cost per uncomplicated case averted, including:
#'   * scenario_name: character
#'   * cost_per_averted_lci: numeric (lower confidence interval)
#'   * cost_per_averted_mean: numeric (mean cost per case averted)
#'   * cost_per_averted_uci: numeric (upper confidence interval)
#' @export
calculate_cost_effectiveness <- function(
  costs_dt,
  sim_dt,
  reference,
  year_start = 2026,
  year_end = 2026
) {
  stopifnot(
    "costs_dt must be a non-empty data frame" = qtest(costs_dt, "D+") &&
      nrow(costs_dt) > 0
  )
  stopifnot(
    "sim_dt must be a non-empty data frame" = qtest(sim_dt, "D+") &&
      nrow(sim_dt) > 0
  )
  stopifnot(
    "reference must be a string" = qtest(reference, "S1") &&
      reference != ""
  )
  stopifnot(
    "year_start must be <= year_end" = year_start <= year_end
  )

  setDT(costs_dt)
  setDT(sim_dt)

  # Validate data table columns
  stopifnot(
    length(
      setdiff(
        c(
          "scenario_name",
          "total_cost_lci",
          "total_cost_mean",
          "total_cost_uci"
        ),
        colnames(costs_dt)
      )
    ) ==
      0
  )
  stopifnot(
    length(
      setdiff(
        c(
          "scenario_name",
          "admin_1",
          "admin_2",
          "EIR_CI",
          "year",
          "cum_nUncomp",
          "cum_nSevere",
          "cum_tUncomp",
          "cum_tSevere",
          "cum_expectedDirectDeaths"
        ),
        colnames(sim_dt)
      )
    ) ==
      0
  )

  # Aggregate impact data to get cumulative uncomplicated cases by scenario and
  # EIR
  data <- aggregate_impact(
    data = sim_dt,
    year_start = year_start,
    year_end = year_end
  )

  # Select only scenario_name and cum_nUncomp columns (uncomplicated cases)
  cols <- c(1, grep("cum_nUncomp", names(data)))
  data <- data[, ..cols]

  # Calculate relative impact (difference from reference). The negative
  # sign converts cases to cases averted (positive values)
  data <- data[,
    c(2:ncol(data)) := lapply(
      .SD,
      function(x) -1 * (x - x[scenario_name == reference])
    ),
    .SDcols = c(2:ncol(data))
  ]

  # Remove reference scenario as it has zero cases averted
  data <- data[!startsWith(scenario_name, reference)]

  # Merge cost data with impact data
  data <- merge(
    x = data,
    y = costs_dt[, c(
      "scenario_name",
      "total_cost_lci",
      "total_cost_mean",
      "total_cost_uci"
    )],
    all.x = TRUE,
    by = "scenario_name"
  )

  # Calculate cost per case averted for each confidence interval
  data <- data[,
    .(
      cost_per_averted_lci = total_cost_lci / cum_nUncomp_lci,
      cost_per_averted_mean = total_cost_mean / cum_nUncomp_mean,
      cost_per_averted_uci = total_cost_uci / cum_nUncomp_uci
    ),
    by = scenario_name
  ]
  return(data)
}

#' Calculate intervention costs based on population coverage and unit costs
#'
#' This function calculates the costs of interventions by multiplying the
#' population covered by each intervention by its unit cost. The calculation is
#' performed for each EIR confidence interval and scenario combination.
#'
#' Note that the order of `interventions`, `intervention_cols` and
#' `intervention_cov` must be the same.
#'
#' @param dt data.table containing simulation data with population and coverage
#'   information. Must include columns:
#'   * scenario_name: character - name of the simulation scenario
#'   * EIR_CI: numeric - EIR confidence interval value
#'   * year: integer - year of simulation
#'   * seed: integer - simulation seed for stochastic runs
#'   * all columns specified in `interventions` - intervention indicator columns
#'   * all columns specified in `intervention_cov` - intervention coverage
#'     columns
#'   * column specified in `pop_col` - population column
#' @param interventions Character vector of intervention names
#' @param intervention_cols Character vector of intervention column names
#' @param intervention_cov Character vector of intervention coverage column
#'   names
#' @param unit_costs Named list of unit costs for each intervention (in chosen
#'   currency). Names must match the intervention names in `interventions`.
#' @param year_start The start year for which to calculate costs (integer)
#' @param year_end The end year for which to calculate costs (integer)
#' @param pop_col Name of population column in dt (default: "nHost")
#' @return data.table with calculated costs per intervention and total costs.
#'   The output includes:
#'   * scenario_name: character - scenario identifier
#'   * For each EIR confidence interval (e.g., "lci", "mean", "uci"):
#'     - {intervention}_pop: numeric - population covered by intervention
#'     - {intervention}_cost: numeric - cost of intervention
#'     - total_cost: numeric - sum of all intervention costs
#'   * Columns are reshaped to wide format with EIR_CI values as suffixes
#' @export
calculate_intervention_costs <- function(
  dt,
  interventions,
  intervention_cols,
  intervention_cov,
  unit_costs,
  year_start = 2026,
  year_end = 2026,
  pop_col = "nHost"
) {
  stopifnot(
    "dt must be a non-empty data frame" = qtest(dt, "D+") && nrow(dt) > 0
  )
  stopifnot(
    "interventions must be a character vector" = qtest(interventions, "S+")
  )
  if (!qtest(intervention_cols, "S+")) {
    stop(
      "intervention_cols must be a character vector"
    )
  }
  if (!qtest(intervention_cov, "S+")) {
    stop(
      "intervention_cov must be a character vector"
    )
  }
  stopifnot("unit_costs must be a non-empty list" = qtest(unit_costs, "L+"))
  stopifnot("year_start must be an integer" = qtest(year_start, "X1"))
  stopifnot("year_end must be an integer" = qtest(year_end, "X1"))
  stopifnot(
    "year_start must be <= year_end" = year_start <= year_end
  )
  stopifnot("pop_col must be a string" = qtest(pop_col, "S1") && pop_col != "")

  # Convert input to data.table if it isn't already
  setDT(dt)

  # Validate year range exists in data
  available_years <- unique(dt$year)
  years_in_range <- available_years[
    available_years >= year_start & available_years <= year_end
  ]
  if (length(years_in_range) == 0) {
    stop(
      "No data available for year range ",
      year_start,
      "-",
      year_end,
      ". Available years: ",
      paste(sort(available_years), collapse = ", ")
    )
  }

  # Validate data table columns
  stopifnot(
    length(
      setdiff(
        c(
          "scenario_name",
          "EIR_CI",
          "year",
          "seed",
          intervention_cols,
          intervention_cov,
          pop_col
        ),
        colnames(dt)
      )
    ) ==
      0
  )

  # Initialize calculation of intervention costs
  costs_dt <- {
    # Create base data.table with unique scenario/EIR combinations
    # This serves as the foundation for accumulating intervention costs
    temp_dt <- unique(dt[, scenario_name, EIR_CI])

    # Loop through each intervention to calculate its costs
    for (i in seq_along(interventions)) {
      # Extract intervention details for current iteration

      # Current intervention name
      int_name <- interventions[i]
      # Column containing intervention data (indicator variable)
      int_col <- intervention_cols[i]
      # Column containing coverage data (proportion covered)
      int_cov_col <- intervention_cov[i]
      # Name for population column (e.g., "itn_pop" for ITN intervention)
      int_pop_name <- paste0(tolower(int_name), "_pop")
      # Name for cost column (e.g., "itn_cost" for ITN intervention)
      int_cost_name <- paste0(tolower(int_name), "_cost")

      # Calculate population and costs for current intervention
      # Step 1: Calculate population covered by intervention across years and
      # seeds Formula: population * intervention_indicator * coverage
      temp_dt <- dt[
        year >= year_start & year <= year_end,
        .(
          int_pop_name = sum(pop_col * int_col * int_cov_col)
        ),
        by = .(scenario_name, EIR_CI, seed),
        env = list(
          pop_col = pop_col,
          int_col = int_col,
          int_cov_col = int_cov_col,
          int_pop_name = int_pop_name
        )
        # Step 2: Average across seeds to get mean population covered
      ][,
        .(int_pop_name = mean(int_pop_name)),
        by = .(scenario_name, EIR_CI),
        env = list(
          int_pop_name = int_pop_name
        )
        # Step 3: Join with base data.table
      ][temp_dt, on = c("scenario_name", "EIR_CI")][,
        # Step 4: Calculate cost by multiplying population by unit cost
        int_cost_name := int_pop_name * unit_costs[[int_name]],
        env = list(
          int_cost_name = int_cost_name,
          int_pop_name = int_pop_name
        )
      ]
    }

    temp_dt
  }

  # Calculate total cost across all interventions

  # Find all cost columns using pattern matching
  cost_cols_idx <- patterns("_cost", cols = names(costs_dt))
  # Get names of cost columns
  cost_cols <- names(costs_dt[, ..cost_cols_idx])
  # Create sum expression by concatenating cost column names with " + "
  total_cost_call <- paste0(cost_cols, collapse = " + ")
  # Add total cost column by evaluating the sum expression
  costs_dt <- costs_dt[,
    # Add total cost column
    total_cost := call,
    # Convert string to expression for evaluation
    env = list(call = str2expression(total_cost_call))
  ]

  # Reshape data from long to wide format for easier analysis
  # Convert from `scenario_name | EIR_CI | cost_columns to scenario_name |
  # cost_columns_lci | cost_columns_mean | cost_columns_uci`
  costs_dt <- dcast(
    costs_dt,
    scenario_name ~ EIR_CI,
    value.var = names(costs_dt)[
      -which(names(costs_dt) %chin% c("scenario_name", "EIR_CI"))
    ]
  )

  # Clean up column names by removing redundant "_EIR_" text
  # Original format: column_EIR_lci becomes column_lci
  setnames(costs_dt, old = 2:length(names(costs_dt)), new = function(x) {
    gsub("_EIR_", "_", x)
  })

  return(costs_dt)
}
