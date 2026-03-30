box::use(
  checkmate[qtest],
  data.table[
    `:=`,
    `.SD`,
    copy,
    data.table,
    melt,
    rbindlist,
    setnames,
    uniqueN
  ],
  logger[log_debug, log_trace],
)

.datatable.aware <- TRUE # nolint
#' Generate counterfactual logical vectors
#'
#' For a named logical vector of active interventions, generates one modified
#' copy per active intervention where that intervention is set to FALSE.
#' Used internally by `per_interv_impact()` to build the set of
#' counterfactual scenarios needed for marginal impact calculation.
#'
#' @param x Named logical vector indicating which interventions are active
#'   (`TRUE` = present, `FALSE` = absent). Names must correspond
#'   to intervention column names in the data.
#' @param base_interventions Character vector of intervention column names that
#'   cannot be switched off (default: CM and ICCM as base minimum).
#'
#' @return A named list of logical vectors. Each element is a copy of
#'   `x` with one active intervention set to `FALSE`. The list
#'   is named by intervention. Base interventions are excluded from
#'   counterfactual generation.
#'
#' @examples
#' \dontrun{
#' x <- c(deployed_int_CM = TRUE, deployed_int_IRS = FALSE, deployed_int_SMC = TRUE)
#' counterfactual_lgc(x)
#' # Returns a list of length 1:
#' #   $deployed_int_SMC: CM = TRUE, IRS = FALSE, SMC = FALSE
#' # CM is not included as it's a base intervention
#' }
#'
#' @keywords internal
#' @noRd
counterfactual_lgc <- function(
  x,
  base_interventions = c("deployed_int_CM", "deployed_int_ICCM")
) {
  stopifnot(
    "x must be a named logical vector" = qtest(x, "B+") &&
      !is.null(names(x))
  )

  # Find indices where x is TRUE AND not a base intervention
  switchable_indices <- which(x & !(names(x) %in% base_interventions))

  res <- lapply(switchable_indices, function(i) {
    y <- x
    y[i] <- FALSE
    names(y) <- names(x)
    y
  })
  names(res) <- names(x)[switchable_indices]
  return(res)
}

#' Filter a data.table to rows matching a logical intervention pattern
#'
#' Subsets a data.table to rows where the intervention columns exactly match
#' the named logical vector `target`. Used internally by
#' `per_interv_impact()` to retrieve the counterfactual scenario row
#' for a specific intervention combination.
#'
#' @param df A data.table containing one logical column per intervention
#'   and additional outcome columns.
#' @param target Named logical vector specifying the exact intervention
#'   combination to match. Names must correspond to column names in `df`.
#' @param interv Character vector of intervention column names to use for
#'   matching.
#'
#' @return A data.table containing only the rows of `df` where all
#'   columns named in `interv` match `target` exactly.
#'
#' @keywords internal
#' @noRd
filter_lgc <- function(
  df,
  target,
  interv = c(
    "deployed_int_LSM", "deployed_int_IRS", "deployed_int_IPTSc",
    "deployed_int_Vaccine", "deployed_int_CM", "deployed_int_ICCM",
    "deployed_int_STD_Nets", "deployed_int_PBO_Nets", "deployed_int_IG2_Nets",
    "deployed_int_SMC", "deployed_int_PMC"
  )
) {
  stopifnot(
    "df must be a data.table" = qtest(df, "D+"),
    "target must be a named logical vector" = qtest(target, "B+") &&
      !is.null(names(target)),
    "interv must be a character vector" = qtest(interv, "S+")
  )

  # Handle empty data.table

if (nrow(df) == 0) {
    return(df)
  }

  # Check that all interv columns exist
  missing_cols <- setdiff(interv, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing intervention columns:", paste(missing_cols, collapse = ", ")))
  }

  subdf <- df[, .SD, .SDcols = interv]
  target_matrix <- matrix(target, nrow(df), length(target), byrow = TRUE)
  df[rowSums(subdf == target_matrix) == length(target)]
}

#' Calculate per-intervention counterfactual impact
#'
#' For a given district x age group x scenario combination, calculates the
#' marginal impact of each active intervention by comparing the full scenario
#' to a counterfactual where that intervention is removed.
#'
#' Impact is defined as:
#'
#' `IMPACT = indicator(without intervention X) - indicator(with intervention X)`
#'
#' A positive value means cases averted by that intervention.
#'
#' @param df A data.table prepared from the simulation output. Must contain
#'   columns for `admin_2`, `age_group`, `scenario_name`,
#'   `cum_nUncomp`, and one logical column per intervention.
#' @param strata Named character vector with three elements: `admin_2`,
#'   `age_group`, `scenario_name`. Identifies the
#'   specific row combination to analyse.
#' @param indicator Character vector of outcome column names to use as the
#'   impact metric. Defaults to `"cum_nUncomp"`.
#' @param interv Character vector of intervention column names.
#' @param keep_col Optional character vector of additional columns to retain
#'   in the output. Default is `NULL`.
#' @param plan Character string specifying the plan for the baseline scenario.
#'   Counterfactuals are always fetched from "Customized" plan.
#' @param counterfactual_plan Character string specifying the plan for
#'   counterfactuals. Defaults to "Customized".
#' @param log_ns Optional logging namespace for debug output.
#'
#' @return A data.table in long format with one row per active intervention,
#'   containing columns: `admin_2`, `age_group`, `scenario_name`,
#'   `intervention`, `metric`, and `value` (the impact).
#'
#' @examples
#' \dontrun{
#' per_interv_impact(
#'   df1,
#'   strata = c(
#'     admin_2 = "Bunda District Council",
#'     age_group = "0-100",
#'     scenario_name = "nsp"
#'   )
#' )
#' }
#'
#' @export
per_interv_impact <- function(
  df,
  strata = c(
    admin_2 = "Bunda District Council",
    age_group = "0-100",
    scenario_name = "nsp"
  ),
  indicator = c("cum_nUncomp"),
  interv = c(
    "deployed_int_LSM", "deployed_int_IRS", "deployed_int_IPTSc",
    "deployed_int_Vaccine", "deployed_int_CM", "deployed_int_ICCM",
    "deployed_int_STD_Nets", "deployed_int_PBO_Nets", "deployed_int_IG2_Nets",
    "deployed_int_SMC", "deployed_int_PMC"
  ),
  keep_col = NULL,
  plan = NULL,
  counterfactual_plan = "Customized",
  log_ns = NULL
) {
  # --- Input validation ---
 stopifnot(
    "df must be a data.table" = qtest(df, "D+"),
    "strata must be a named character vector with at least 3 elements" =
      qtest(strata, "S+") && length(strata) >= 3 && !is.null(names(strata)),
    "indicator must be a character vector" = qtest(indicator, "S+"),
    "interv must be a character vector" = qtest(interv, "S+")
  )

  # --- Extract strata info ---
  admin_2_val <- strata[["admin_2"]]
  age_group_val <- strata[["age_group"]]
  scenario_name_val <- strata[["scenario_name"]]

  # --- Define columns needed ---
  join_cols <- c("year", "seed")
  needed_cols <- unique(c(
    "admin_2", "age_group", "scenario_name",
    indicator, interv, join_cols,
    if (!is.null(plan)) "plan" else NULL,
    keep_col
  ))

  # --- Step 1: Get baseline data (from selected plan + scenario) ---
  # Store plan arg in differently-named var to avoid data.table column name collision
  plan_filter <- plan

  baseline_dt <- df[
    admin_2 == admin_2_val & age_group == age_group_val &
      scenario_name == scenario_name_val
  ]
  if (!is.null(plan_filter) && "plan" %in% names(baseline_dt)) {
    baseline_dt <- baseline_dt[plan == plan_filter]
  }
  baseline_dt <- baseline_dt[, .SD, .SDcols = intersect(needed_cols, names(df))]

  if (nrow(baseline_dt) == 0) {
    if (!is.null(log_ns)) {
      log_trace(
        paste("No baseline data found for:", admin_2_val, age_group_val, scenario_name_val),
        namespace = log_ns
      )
    }
    return(data.table())
  }

  # --- Step 2: Get target intervention pattern from baseline ---
  target <- as.logical(unlist(baseline_dt[1, ..interv]))
  names(target) <- interv

  if (!is.null(log_ns)) {
    active_intervs <- names(target)[target]
    log_trace(
      paste("Target interventions:", paste(active_intervs, collapse = ", ")),
      namespace = log_ns
    )
  }

  # --- Step 3: Generate counterfactual patterns ---
  cf_patterns <- counterfactual_lgc(
    target,
    base_interventions = c("deployed_int_CM", "deployed_int_ICCM")
  )

  if (length(cf_patterns) == 0) {
    if (!is.null(log_ns)) {
      log_trace("No switchable interventions found", namespace = log_ns)
    }
    return(data.table())
  }

  if (!is.null(log_ns)) {
    log_trace(
      paste("Generated", length(cf_patterns), "counterfactual patterns:",
            paste(names(cf_patterns), collapse = ", ")),
      namespace = log_ns
    )
  }

  # --- Step 4: Get counterfactual data (from Customized plan) ---
  cf_dt <- df[admin_2 == admin_2_val & age_group == age_group_val]
  if (!is.null(counterfactual_plan) && "plan" %in% names(cf_dt)) {
    cf_dt <- cf_dt[plan == counterfactual_plan]
  }
  cf_dt <- cf_dt[, .SD, .SDcols = intersect(needed_cols, names(df))]

  if (nrow(cf_dt) == 0) {
    if (!is.null(log_ns)) {
      log_trace(
        paste("No counterfactual data found in plan:", counterfactual_plan),
        namespace = log_ns
      )
    }
    return(data.table())
  }

  if (!is.null(log_ns)) {
    log_trace(
      paste("Counterfactual pool:", nrow(cf_dt), "rows,",
            uniqueN(cf_dt$scenario_name), "scenarios"),
      namespace = log_ns
    )
  }

  # --- Step 5: Calculate impact for each counterfactual pattern ---
  results <- lapply(names(cf_patterns), function(cf_name) {
    cf_pattern <- cf_patterns[[cf_name]]

    # Filter to rows matching this counterfactual pattern
    cf_matched <- filter_lgc(cf_dt, cf_pattern, interv)

    if (nrow(cf_matched) == 0) {
      if (!is.null(log_ns)) {
        log_trace(
          paste("No match for counterfactual:", cf_name),
          namespace = log_ns
        )
      }
      return(NULL)
    }

    # Deduplicate counterfactual by join keys
    cf_matched <- unique(cf_matched, by = c("admin_2", "age_group", join_cols))

    if (!is.null(log_ns)) {
      log_trace(
        paste("Counterfactual", cf_name, "matched", nrow(cf_matched), "rows"),
        namespace = log_ns
      )
    }

    # Join counterfactual with baseline on admin_2, age_group, year, seed
    # Use merge for clarity (avoids key mutation)
    joined <- merge(
      cf_matched,
      baseline_dt,
      by = c("admin_2", "age_group", join_cols),
      suffixes = c("_cf", "_baseline"),
      allow.cartesian = FALSE
    )

    if (nrow(joined) == 0) {
      if (!is.null(log_ns)) {
        log_trace(
          paste("No join matches for:", cf_name),
          namespace = log_ns
        )
      }
      return(NULL)
    }

    # Calculate impact for each indicator
    # IMPACT = cases(counterfactual) - cases(baseline)
    # Positive = intervention averted cases
    for (ind in indicator) {
      cf_col <- paste0(ind, "_cf")
      baseline_col <- paste0(ind, "_baseline")
      impact_col <- paste0("IMPACT_", ind)
      joined[, (impact_col) := get(cf_col) - get(baseline_col)]
    }

    if (!is.null(log_ns)) {
      impact_col <- paste0("IMPACT_", indicator[1])
      mean_impact <- mean(joined[[impact_col]], na.rm = TRUE)
      log_trace(
        paste("Intervention", cf_name, "- mean impact:", round(mean_impact, 2)),
        namespace = log_ns
      )
    }

    # Aggregate impact: take final year (cumulative indicator), then mean across seeds
    # cum_nUncomp is cumulative - we want the final year value, not sum of all years
    impact_cols <- paste0("IMPACT_", indicator)
    base_cols <- c("admin_2", "age_group", "scenario_name_baseline")
    if (!is.null(keep_col)) {
      keep_col_baseline <- paste0(keep_col, "_baseline")
      keep_col_baseline <- intersect(keep_col_baseline, names(joined))
      base_cols <- c(base_cols, keep_col_baseline)
    }
    base_cols <- intersect(base_cols, names(joined))

    # Step 1: Take final year (max year) within each seed
    # Since indicator is cumulative, final year = total impact over simulation
    final_year_impacts <- joined[
      joined[, .I[year == max(year)], by = c(base_cols, "seed")]$V1
    ]

    # Step 2: Mean across seeds to get expected total impact
    result <- final_year_impacts[,
      lapply(.SD, mean, na.rm = TRUE),
      by = base_cols,
      .SDcols = impact_cols
    ]

    if (!is.null(log_ns)) {
      log_trace(
        paste("Aggregated to", nrow(result), "rows (1 per district)"),
        namespace = log_ns
      )
    }

    # Melt to long format
    result <- melt(
      result,
      id.vars = setdiff(names(result), impact_cols),
      measure.vars = impact_cols,
      variable.name = "metric",
      value.name = "value"
    )

    # Add intervention name and clean up
    result[, intervention := cf_name]
    if ("scenario_name_baseline" %in% names(result)) {
      setnames(result, "scenario_name_baseline", "scenario_name")
    }

    result
  })

  # Combine results
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(data.table())
  }

  result <- rbindlist(results, fill = TRUE)

  # Clean up IMPACT columns if present (they're now in 'value')
  impact_cols <- intersect(paste0("IMPACT_", indicator), names(result))
  if (length(impact_cols) > 0) {
    result[, (impact_cols) := NULL]
  }

  return(result)
}

#' Assign rank group label from numeric rank
#'
#' Converts a numeric rank into a paired rank group label.
#' Used to colour districts consistently across map and bar chart.
#'
#' @param r Integer vector of ranks where 1 = highest impact district.
#'
#' @return Character vector of rank group labels:
#'   `"1-2"`, `"3-4"`, `"5-6"`, `"7-8"`, or `"9-10"`.
#'
#' @examples
#' \dontrun{
#' rank_groups(c(1, 2, 3, 7, 10))
#' # Returns: "1-2" "1-2" "3-4" "7-8" "9-10"
#' }
#'
#' @export
rank_groups <- function(r) {
  stopifnot(
    "r must be a numeric vector" = qtest(r, "N+")
  )

  data.table::fcase(
    r <= 2L, "1-2",
    r <= 4L, "3-4",
    r <= 6L, "5-6",
    r <= 8L, "7-8",
    default = "9-10"
  )
}

#' Assign numeric rank group index from numeric rank
#'
#' Converts a numeric rank into an integer group index (1 to 5).
#' Used internally for ordering and colour mapping.
#'
#' @param r Integer vector of ranks where 1 = highest impact district.
#'
#' @return Integer vector from `1L` (rank group 1-2) to
#'   `5L` (rank group 9-10).
#'
#' @examples
#' \dontrun{
#' rank_numeric(c(1, 3, 5, 8, 10))
#' # Returns: 1 2 3 4 5
#' }
#'
#' @export
rank_numeric <- function(r) {
  stopifnot(
    "r must be a numeric vector" = qtest(r, "N+")
  )

  data.table::fcase(
    r <= 2L, 1L,
    r <= 4L, 2L,
    r <= 6L, 3L,
    r <= 8L, 4L,
    default = 5L
  )
}
