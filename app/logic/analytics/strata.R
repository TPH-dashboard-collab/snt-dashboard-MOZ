# fmt: skip
box::use(
  checkmate[qtest],
  data.table[as.data.table, dcast, setorder, `:=`, `.N`, `.SD`],
  stats[setNames],
)

# fmt: skip
box::use(
  app/logic/core/varfns[classify_risk, high_risk_strata, risk_levels],
)

#' Calculate risk strata from prevalence data
#'
#' Assigns risk stratum categories based on prevalence rate thresholds
#' defined in config.yml.
#'
#' @param data data.table containing a `prevalenceRate` column
#' @return data.table with added `risk_stratum` factor column
#' @export
calculate_risk_strata <- function(data) {
  dt <- as.data.table(data)
  dt[, risk_stratum := factor(
    classify_risk(prevalenceRate),
    levels = risk_levels()
  )]
  dt[]
}

#' Aggregate simulation data for risk strata analysis
#'
#' Aggregates data across multiple seeds and filters for target age groups.
#' Results are averaged within admin units, years, and plans.
#'
#' @param df data.table containing simulation results with columns:
#'   * age_group: character
#'   * admin_1: character
#'   * admin_2: character
#'   * year: integer
#'   * plan: character
#'   * prevalenceRate: numeric
#'   * nHost: numeric (population)
#' @param age_filter Age group to filter for (default: "0-5")
#' @return Aggregated data.table with risk strata assigned
#' @export
aggregate_strata_data <- function(df, age_filter = "0-5") {
  required_cols <- c(
    "age_group",
    "admin_1",
    "admin_2",
    "year",
    "plan",
    "prevalenceRate",
    "nHost"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  dt <- as.data.table(df)
  agg <- dt[
    age_group == age_filter,
    .(
      prevalenceRate = mean(prevalenceRate, na.rm = TRUE),
      nHost = mean(nHost, na.rm = TRUE)
    ),
    by = .(admin_1, admin_2, year, plan)
  ]
  calculate_risk_strata(agg)
}

#' Prepare persistence data for districts staying in high-risk strata
#'
#' Identifies districts that remained in High or Moderate risk strata between
#' start and end years, calculating prevalence changes.
#'
#' @param df data.table with risk strata data containing columns:
#'   * admin_1: character
#'   * admin_2: character
#'   * year: integer
#'   * risk_stratum: factor
#'   * prevalenceRate: numeric
#' @param selected_years Numeric vector of years (uses min and max)
#' @return data.table with persistence data or NULL if insufficient data
#' @export
prep_persistence_data <- function(df, selected_years) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  year_start <- min(selected_years)
  year_end <- max(selected_years)

  col_start_risk <- paste0("risk_stratum_", year_start)
  col_end_risk <- paste0("risk_stratum_", year_end)
  col_start_prev <- paste0("prevalenceRate_", year_start)
  col_end_prev <- paste0("prevalenceRate_", year_end)

  dt <- as.data.table(df)

  # Keep only the two boundary years and relevant columns
  # Aggregate to handle multiple plans - take mean prevalence per district/year
  dt_long <- dt[
    year %in% c(year_start, year_end),
    .(
      prevalenceRate = mean(prevalenceRate, na.rm = TRUE)
    ),
    by = .(admin_1, admin_2, year)
  ]

  # Recalculate risk_stratum based on aggregated prevalence
  dt_long[, risk_stratum := classify_risk(prevalenceRate)]

  # Pivot wide: separate risk_stratum and prevalenceRate columns per year
  wide_dt <- dcast(
    dt_long,
    admin_1 + admin_2 ~ year,
    value.var = c("risk_stratum", "prevalenceRate")
  )

  required_cols <- c(col_start_risk, col_end_risk, col_start_prev, col_end_prev)
  if (!all(required_cols %in% names(wide_dt))) {
    return(NULL)
  }

  # Compute absolute PfPR change and keep only persistently elevated districts
  wide_dt[, PfPR_Change := get(col_end_prev) - get(col_start_prev)]

  res <- wide_dt[
    get(col_start_risk) %in%
      high_risk_strata() &
      get(col_end_risk) %in% high_risk_strata()
  ]

  res[]
}

#' Prepare data for risk strata heatmap visualization
#'
#' Creates a structured data.table for heatmap display, ordering districts
#' by dominant stratum, persistence, and end-year prevalence.
#'
#' @param df data.table with risk strata data containing columns:
#'   * admin_2: character
#'   * year: integer
#'   * risk_stratum: character/factor
#'   * prevalenceRate: numeric
#' @param selected_years Numeric vector of years to include
#' @return data.table formatted for heatmap visualization or NULL if no data
#' @export
prep_heatmap_data <- function(df, selected_years) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  years <- sort(selected_years)
  total_yrs <- length(years)
  year_end <- max(years)

  dt <- as.data.table(df)

  # Aggregate to handle multiple plans - take mean prevalence per district/year
  dt_filtered <- dt[
    year %in% years,
    .(prevalenceRate = mean(prevalenceRate, na.rm = TRUE)),
    by = .(admin_2, year)
  ]

  # Recalculate risk_stratum based on aggregated prevalence
  dt_filtered[, risk_stratum := classify_risk(prevalenceRate)]
  dt_filtered[, is_high_mod := risk_stratum %in% high_risk_strata()]

  if (nrow(dt_filtered) == 0) {
    return(NULL)
  }

  # Years each district spent in High/Moderate (used in tooltip)
  persistence_score <- dt_filtered[,
    .(persist_years = sum(is_high_mod, na.rm = TRUE)),
    by = admin_2
  ]

  # Dominant stratum = most frequently occurring stratum across selected years
  strat_counts <- dt_filtered[, .N, by = .(admin_2, risk_stratum)]
  setorder(strat_counts, admin_2, -N)
  dominant <- strat_counts[, .SD[1], by = admin_2][, .(
    admin_2,
    dominant_stratum = risk_stratum
  )]

  # Years each district spent specifically in its dominant stratum
  dt_dom <- merge(dt_filtered, dominant, by = "admin_2", all.x = TRUE)
  dominant_years <- dt_dom[,
    .(dom_years = sum(risk_stratum == dominant_stratum, na.rm = TRUE)),
    by = admin_2
  ]

  # End-year prevalence from the already-filtered data (correct plan/age/region)
  end_prev <- unique(
    dt_filtered[year == year_end, .(admin_2, end_prevalence = prevalenceRate)],
    by = "admin_2"
  )

  # Stratum rank for ordering: High = 1 (top), Very Low = 4 (bottom)
  stratum_rank <- setNames(seq_along(risk_levels()), rev(risk_levels()))

  # Build ordering table: stratum rank → years in dominant stratum → end prevalence
  order_dt <- merge(dominant, dominant_years, by = "admin_2", all.x = TRUE)
  order_dt <- merge(order_dt, end_prev, by = "admin_2", all.x = TRUE)
  order_dt[, stratum_rank := stratum_rank[dominant_stratum]]
  setorder(order_dt, stratum_rank, -dom_years, -end_prevalence)

  # Plotly y-axis runs bottom-up, so reverse the ordering for the factor levels
  ordered_levels <- rev(order_dt$admin_2)

  # Join all tooltip columns back onto the long filtered table
  dt_out <- merge(dt_filtered, persistence_score, by = "admin_2", all.x = TRUE)
  dt_out <- merge(dt_out, dominant, by = "admin_2", all.x = TRUE)
  dt_out <- merge(dt_out, end_prev, by = "admin_2", all.x = TRUE)
  dt_out[, total_years := total_yrs]
  dt_out[, admin_2 := factor(admin_2, levels = ordered_levels)]

  dt_out[]
}
