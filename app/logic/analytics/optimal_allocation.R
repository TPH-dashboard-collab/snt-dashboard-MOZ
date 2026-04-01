box::use(
  checkmate[qtest],
  data.table,
  ggplot2,
  lpSolve,
  sf
)

# fmt: skip
box::use(
  app/logic/visualization/formatters[format_large_number],
)

# TODO: Break up file:
#   - Move Maps
#   - Move other plots

# TODO: REview functions:
#   - Input validation, correctness
#   - Performance

# Thes are pure helper functions used by my s server.R.
# All functions are stateless (no Shiny reactives or globals).
# Sources:
#   - data.table for all tabular operations
#   - lpSolve for budget optimisation
#   - ggplot2 for tornado charts
#   - sf / leaflet for map rendering

# THE OPTIMISATION FUNCTION

#' Solve a budget-constrained resource allocation problem using linear
#' programming
#'
#' @description Given a set of districts (regions) and candidate intervention
#'   scenarios (policies) with associated costs and health outcomes, this
#'   function selects exactly one scenario per district that maximises (or
#'   minimises) the total health objective while keeping total cost within the
#'   budget envelope.
#'
#' The LP formulation:
#'   * Binary decision variable x[r,p] = 1 if policy p is chosen for region r
#'   * Objective: max/min sum[r,p] health[r,p] * x[r,p]
#'   * Budget: sum[r,p] cost[r,p] * x[r,p] <= budget_env
#'   * Uniqueness: sum[p] x[r,p] = 1 for each region r
#'
#' @param df data.frame or data.table with one row per region-policy pair. Must
#'   contain columns named by region_name, policy_name, cost_name, and
#'   health_name.
#' @param budget_env Numeric scalar. Total budget available across all regions
#'   (USD).
#' @param region_name Character. Column name identifying the geographic unit
#'   (default "admin_2").
#' @param policy_name Character. Column name identifying the intervention
#'   scenario (default "scenario_name").
#' @param cost_name Character. Column name for the cost of each scenario per
#'   region (default "avg_cost").
#' @param health_name Character. Column name for the health metric to optimise
#'   (default "averted"). Must be numeric.
#' @param optim_dir Character. Direction of optimisation: "max" (default) or
#'   "min".
#'
#' @return A data.table containing the selected rows from df (one per region),
#'   with original columns intact plus `policy_allocation` (approximately 1 for
#'   selected rows). Returns an empty data.table if no feasible solution exists.
#'
#' @examples
#' \dontrun{
#'   result <- optimal_allocation(
#'     df          = metrics_mean,
#'     budget_env  = 71000000,
#'     health_name = "averted"
#'   )
#' }
#' @export
optimal_allocation <- function(
  df,
  budget_env,
  region_name = "admin_2",
  policy_name = "scenario_name",
  cost_name = "avg_cost",
  health_name = "averted",
  optim_dir = "max"
) {
  # Work on an internal copy to avoid modifying the caller's data
  # REVIEW: Is copy necessary here?
  working_df <- data.table$copy(data.table$as.data.table(df))
  data.table$setnames(
    working_df,
    old = c(region_name, policy_name),
    new = c("region", "policy")
  )

  # Count policies per region and number of distinct regions
  working_df[, N_policies := .N, by = region]
  working_df[, N_regions := data.table$uniqueN(region)]

  # Build index table: start/stop row positions for each region in the LP matrix
  df_idx <- unique(working_df[, .(region, N_policies)])[,
    `:=`(
      start = cumsum(N_policies) - N_policies + 1L,
      stop = cumsum(N_policies)
    )
  ]

  # LP objective: health metric for every region-policy combination
  # Scale objective to avoid numerical failure (lpSolve status 5) when

  # coefficient magnitudes are large (e.g., millions of cases averted).
  objective_raw <- working_df[[health_name]]
  obj_scale <- max(abs(objective_raw), 1)
  objective_coeffs <- objective_raw / obj_scale

  # Budget constraint row: cost of each region-policy combination
  constr_budget <- matrix(working_df[[cost_name]], nrow = 1, byrow = TRUE)

  # Uniqueness constraints: one row per region, 1s spanning that region's
  # policies
  n_regions <- unique(working_df$N_regions)
  n_combos <- nrow(working_df)
  constr_one <- matrix(0L, nrow = n_regions, ncol = n_combos)
  for (i in seq_len(n_regions)) {
    constr_one[i, df_idx$start[i]:df_idx$stop[i]] <- 1L
  }

  # Solve the LP
  solution <- lpSolve$lp(
    direction = optim_dir,
    objective.in = objective_coeffs,
    const.mat = rbind(constr_budget, constr_one),
    const.dir = c("<=", rep("=", n_regions)),
    const.rhs = c(budget_env, rep(1L, n_regions))
  )

  # Attach allocation flag and return selected rows only
  working_df[, policy_allocation := solution$solution]
  return(working_df[policy_allocation > 0.99])
}

#' Classify an optimal district allocation relative to the reference plan
#'
#' @description Compares the number of active interventions in the optimal
#'   scenario against the reference scenario and assigns one of four policy
#'   labels:
#'   * "Reference" - optimizer chose the same scenario as the reference
#'   * "Added" - optimal has more interventions than reference
#'   * "Reduced" - optimal has fewer interventions than reference
#'   * "Substituted" - optimal has a different mix at similar count
#'
#' @param opt_combo Character. Intervention combo string for the optimal plan
#'   (e.g. "ICCM + PBO_Nets"). Compared at the district level.
#' @param ref_combo Character. Intervention combo string for the reference plan
#'   in the same district (e.g. "LSM + CM + PBO_Nets").
#'
#' @return Character scalar: one of "Reference", "Added", "Reduced",
#'   "Substituted".
#'
#' @examples
#' \dontrun{
#'   classify_district("ICCM + PBO_Nets", "LSM + CM + PBO_Nets")
#'   # Returns "Reduced" (2 interventions vs 3)
#' }
#' @export
classify_district <- function(opt_combo, ref_combo) {
  # Guard against NA or missing values passed from mapply
  if (is.na(opt_combo) || length(opt_combo) == 0L) {
    return("Reference")
  }
  if (is.na(ref_combo) || length(ref_combo) == 0L) {
    return("Reference")
  }

  # Exact match, no change from reference
  if (opt_combo == ref_combo) {
    return("Reference")
  }

  count_ints <- function(combo) {
    if (combo == "No intervention") 0L else length(strsplit(combo, "\\+")[[1]])
  }

  opt_n <- count_ints(opt_combo)
  ref_n <- count_ints(ref_combo)

  if (opt_n > ref_n) {
    return("Added")
  }
  if (opt_n < ref_n) {
    return("Reduced")
  }
  return("Substituted")
}


#  METRICS COMPUTATION FUNCTION

#' Compute per-scenario cost and case metrics across all districts
#'
#' @description Operates on fully pre-aggregated data from
#'   `get_ee_district_summaries()` (one row per scenario-district-EIR_CI).
#'   Applies unit costs to intervention volumes and computes period cases.
#'   Does NOT compute incremental metrics — use `compute_incremental_metrics()`
#'   for that.
#'
#' @param data data.table. Pre-aggregated output from
#'   `get_ee_district_summaries()`, with columns: scenario_name, plan, admin_2,
#'   EIR_CI, {int}_pop (mean intervention population per intervention),
#'   cum_nUncomp_end, cum_nUncomp_start.
#' @param int_names Character vector. Intervention names (e.g. "CM", "ICCM").
#' @param u_costs Named list of numeric unit costs, one entry per intervention
#'   name (same names as `int_names`).
#'
#' @return data.table with one row per scenario-district-EIR_CI combination and
#'   columns: scenario_name, plan, admin_2, EIR_CI, avg_cost, averted_period
#'   (plus all original columns).
#'
#' @export
compute_metrics <- function(data, int_names, u_costs) {
  # {int}_pop = mean_over_seeds(SUM(nHost * deployed * coverage)) from the DB
  data[, avg_cost := 0]
  for (int in int_names) {
    pop_col <- paste0(tolower(int), "_pop")
    if (pop_col %in% names(data)) {
      data[, avg_cost := avg_cost + (get(pop_col) * u_costs[[int]])]
    }
  }

  data[,
    averted_period := cum_nUncomp_end -
      data.table$fifelse(is.na(cum_nUncomp_start), 0, cum_nUncomp_start)
  ]

  return(data)
}


#' Compute incremental health and economic metrics vs a reference
#'
#' @description Takes per-scenario metrics from `compute_metrics()` and a
#'   reference dataset, computes incremental cost, cases averted, NMB, ICER.
#'
#' @param metrics data.table. Output of `compute_metrics()` with columns:
#'   scenario_name, plan, admin_2, EIR_CI, avg_cost, averted_period.
#' @param ref data.table. Reference baseline with columns: admin_2, EIR_CI,
#'   avg_cost, averted_period. One row per (admin_2, EIR_CI).
#' @param wtp Numeric. Willingness-to-pay threshold (USD per case averted).
#'
#' @return data.table with all columns from `metrics` plus: r_c, r_cost,
#'   averted, cost_diff, NMB, is_CE, ICER.
#'
#' @export
compute_incremental_metrics <- function(metrics, ref, wtp) {
  ref <- ref[, .(admin_2, EIR_CI, r_c = averted_period, r_cost = avg_cost)]

  m <- merge(metrics, ref, by = c("admin_2", "EIR_CI"), all.x = TRUE)

  m[, `:=`(
    averted = r_c - averted_period,
    cost_diff = avg_cost - r_cost
  )]

  m[, `:=`(
    NMB = (averted * wtp) - cost_diff,
    is_CE = ((averted * wtp) - cost_diff) >= 0
  )]
  m[, ICER := cost_diff / averted]

  return(m)
}


#' Compute budget envelope from a base cost
#'
#' @description Applies a percentage adjustment to the reference plan's total
#'   cost to produce the budget envelope for the optimizer.
#'
#' @param base_cost Numeric. Total cost of the selected reference plan (USD).
#' @param budget_adj Numeric. Percentage adjustment to apply
#'   (e.g. 10 means +10%, -5 means -5%).
#'
#' @return Named list with:
#'   * `curr`: Numeric. Reference base budget (USD).
#'   * `env`: Numeric. Adjusted budget envelope = curr x (1 + budget_adj/100).
#'   * `adj_pct`: Numeric. The percentage adjustment supplied.
#'
#' @export
compute_budget <- function(base_cost, budget_adj) {
  stopifnot(
    "base_cost must be a single positive number" = is.numeric(base_cost) &&
      length(base_cost) == 1L &&
      base_cost > 0
  )

  list(
    curr = base_cost,
    env = base_cost * (1 + budget_adj / 100),
    adj_pct = budget_adj
  )
}


#  MAP HELPERS

#' Join district-level data to the Tanzania shapefile for mapping
#'
#' @description Performs a left join between the shapefile and a data subset,
#'   matching on an upper-cased, trimmed version of the `admin_2` district name.
#'   Returns an sf object suitable for use in `leaflet()` or `ggplot()`.
#'
#' @param data_subset data.table or data.frame containing at minimum an
#'   `admin_2` column.
#' @param shape_file sf object. The Tanzania district shapefile, already
#'   transformed to WGS84 and containing a `join_id` column (upper-cased
#'   admin_2).
#'
#' @return sf object (same CRS as shape_file) with all columns from both the
#'   shapefile and `data_subset`.
#'
#' @export
build_map_obj <- function(data_subset, shape_file) {
  # Standardise join key: upper-case, trimmed district name
  data_clean <- data.table$as.data.table(data_subset)
  data_clean[, join_id := toupper(trimws(as.character(admin_2)))]

  # Join using data.table and immediately wrap in st_as_sf to preserve geometry
  # This performs a left join (keeping all rows from shape_file)
  merged_data <- data_clean[
    data.table$as.data.table(shape_file),
    on = "join_id"
  ]

  sf$st_as_sf(merged_data)
}

#' Prepare long-format data for the facet intervention map
#'
#' @description Takes the LP-selected scenarios and the raw data, derives
#'   intervention group status for each district (CM & ICCM, Nets, PMC & SMC,
#'   LSM, Vaccine, IPTSc, IRS), and returns a long-format data.table with one
#'   row per district x intervention group, excluding "None" rows.
#'
#' @param opt_choices data.table. Best LP allocation with columns `region`
#'   (district) and `policy` (scenario_name).
#' @param data_tza1 data.table. Full raw dataset; used to look up intervention
#'   active flags for the first year of simulation.
#' @param intervention_cols Character vector. Names of all `deployed_int_*`
#'   columns in `data_tza1`.
#'
#' @return data.table with columns: admin_2, Group (intervention category),
#'   Status (e.g. "CM Only", "PBO Nets"). Rows with Status == "None" are
#'   excluded.
#'
#' @export
prepare_facet_data <- function(opt_choices, data_tza1, intervention_cols) {
  # Extract active intervention flags. Data is already fully pre-aggregated
  # by (scenario, admin_2, EIR_CI) — no year or age_group dimensions remain.
  scen_info <- unique(
    data_tza1[,
      .SD,
      .SDcols = c("admin_2", "scenario_name", intervention_cols)
    ],
    by = c("admin_2", "scenario_name")
  )

  # Merge LP selections with intervention flags
  plot_dt <- merge(
    opt_choices[, .(admin_2 = region, scenario_name = policy)],
    scen_info,
    by = c("admin_2", "scenario_name"),
    all.x = TRUE
  )

  # Derive intervention group status labels
  plot_dt[,
    `CM & ICCM` := data.table$fcase(
      deployed_int_CM == TRUE & deployed_int_ICCM == TRUE,
      "CM + iCCM",
      deployed_int_CM == TRUE,
      "CM Only",
      deployed_int_ICCM == TRUE,
      "iCCM Only",
      default = "None"
    )
  ]

  plot_dt[,
    `PMC & SMC` := data.table$fcase(
      deployed_int_PMC == TRUE & deployed_int_SMC == TRUE,
      "PMC + SMC",
      deployed_int_PMC == TRUE,
      "PMC Only",
      deployed_int_SMC == TRUE,
      "SMC Only",
      default = "None"
    )
  ]

  plot_dt[,
    Nets := data.table$fcase(
      (deployed_int_IG2_Nets + deployed_int_PBO_Nets + deployed_int_STD_Nets) >
        1L,
      "Multiple Nets",
      deployed_int_IG2_Nets == TRUE,
      "IG2 Nets",
      deployed_int_PBO_Nets == TRUE,
      "PBO Nets",
      deployed_int_STD_Nets == TRUE,
      "STD Nets",
      default = "None"
    )
  ]

  plot_dt[, LSM := data.table$fifelse(deployed_int_LSM == TRUE, "LSM", "None")]
  plot_dt[,
    Vaccine := data.table$fifelse(
      deployed_int_Vaccine == TRUE,
      "Vaccine",
      "None"
    )
  ]
  plot_dt[,
    IPTSc := data.table$fifelse(deployed_int_IPTSc == TRUE, "IPTSc", "None")
  ]
  plot_dt[, IRS := data.table$fifelse(deployed_int_IRS == TRUE, "IRS", "None")]

  # Pivot to long format: one row per district × intervention group
  groups <- c(
    "CM & ICCM",
    "PMC & SMC",
    "Nets",
    "LSM",
    "Vaccine",
    "IPTSc",
    "IRS"
  )
  keep <- c("admin_2", groups)
  long <- data.table$melt(
    plot_dt[, ..keep],
    id.vars = "admin_2",
    variable.name = "Group",
    value.name = "Status"
  )

  # Drop "None" rows — only districts with active interventions appear on map
  long[Status != "None"]
}


# PLOTTING

#' Build a dual-axis tornado chart of cost vs cases averted
#'
#' @description For a given LP result, groups districts by their selected
#'   intervention combination and draws a horizontal bar chart with:
#'   * Left (gold) bars: total plan cost in M USD (scaled to share the x-axis
#'     with the right-side bars)
#'   * Right (purple) bars: total cases averted vs reference
#'
#' Rows are sorted by cases averted ascending so the best-performing combination
#' appears at the top. Negative bars indicate the optimizer selected a scenario
#' worse than the reference in health terms (can occur when optimising on NMB
#' with a low WTP threshold).
#'
#' @param opt_res data.table. LP result from `optimal_allocation()`, containing
#'   columns: region, policy, averted, avg_cost.
#' @param scen_lookup data.table. Pre-computed scenario summary lookup with
#'   columns: scenario_name, intervention_combo. Used to label rows by
#'   intervention combination.
#'
#' @return A `ggplot` object. Render with `print()` or inside `renderPlot()`.
#'
#' @export
make_tornado <- function(opt_res, scen_lookup) {
  # Build district-level data from LP result
  plot_dt <- data.table$data.table(
    admin_2 = opt_res$region,
    scenario_name = opt_res$policy,
    cases_averted = opt_res$averted,
    avg_cost = opt_res$avg_cost
  )

  # Attach human-readable intervention combination label
  plot_dt <- merge(
    plot_dt,
    unique(scen_lookup[, .(admin_2, scenario_name, intervention_combo)]),
    by = c("admin_2", "scenario_name"),
    all.x = TRUE
  )
  plot_dt[,
    combo := data.table$fifelse(
      is.na(intervention_combo) |
        intervention_combo == "No additional interventions",
      "Reference only",
      intervention_combo
    )
  ]

  # Aggregate across districts sharing the same intervention combination
  tornado_dt <- plot_dt[,
    .(
      cases_averted = sum(cases_averted, na.rm = TRUE),
      total_cost = sum(avg_cost, na.rm = TRUE)
    ),
    by = combo
  ]

  # Sort ascending so highest-performing combo appears at top of chart
  tornado_dt <- tornado_dt[order(cases_averted)]
  tornado_dt[, combo := factor(combo, levels = unique(combo))]

  # Scale factor: maps cost onto the same x-axis range as cases averted
  max_cases <- max(abs(tornado_dt$cases_averted), na.rm = TRUE)
  max_cost <- max(abs(tornado_dt$total_cost), na.rm = TRUE)
  k <- if (max_cost > 0) max_cases / max_cost else 1

  # Negative cost_scaled pushes gold bars to the left of the zero line
  tornado_dt[, cost_scaled := -total_cost * k]

  # Format labels using shared formatter
  tornado_dt[, cost_label := format_large_number(abs(total_cost))]
  tornado_dt[,
    cases_label := format_large_number(abs(cases_averted), prefix = "")
  ]

  # Vertical nudge to prevent bar overlap
  nudge <- 0.15

  ggplot2$ggplot(tornado_dt, ggplot2$aes(y = combo)) +
    # Gold bars — cost, extending left from zero
    ggplot2$geom_col(
      ggplot2$aes(x = cost_scaled),
      fill = "#E8A020",
      width = 0.3,
      position = ggplot2$position_nudge(y = nudge)
    ) +
    # Purple bars — cases averted, extending right from zero
    ggplot2$geom_col(
      ggplot2$aes(x = cases_averted),
      fill = "#4B0082",
      width = 0.3,
      position = ggplot2$position_nudge(y = -nudge)
    ) +
    # Cost labels just outside left bars
    ggplot2$geom_text(
      ggplot2$aes(x = cost_scaled, label = cost_label),
      hjust = 1.08,
      size = 3.2,
      color = "gray20",
      position = ggplot2$position_nudge(y = nudge)
    ) +
    # Cases averted labels just outside right bars
    ggplot2$geom_text(
      ggplot2$aes(
        x = cases_averted,
        label = cases_label,
        hjust = ifelse(cases_averted >= 0, -0.08, 1.08)
      ),
      size = 3.2,
      color = "gray20",
      position = ggplot2$position_nudge(y = -nudge)
    ) +
    ggplot2$geom_vline(xintercept = 0, color = "gray40", linewidth = 0.6) +
    ggplot2$scale_x_continuous(
      labels = function(x) {
        ifelse(
          x >= 0,
          format_large_number(x, prefix = ""),
          format_large_number(abs(x) / k)
        )
      },
      expand = ggplot2$expansion(mult = c(0.25, 0.2))
    ) +
    ggplot2$labs(
      x = NULL,
      y = NULL,
      caption = paste0(
        "\u25A0 Purple = Cases Averted (right)     ",
        "\u25A0 Gold = Cost in USD (left)"
      )
    ) +
    ggplot2$theme_minimal(base_size = 11) +
    ggplot2$theme(
      panel.grid.major.y = ggplot2$element_blank(),
      panel.grid.minor = ggplot2$element_blank(),
      plot.caption = ggplot2$element_text(hjust = 0.5, color = "gray30"),
      plot.margin = ggplot2$margin(8, 45, 8, 8)
    )
}


#' Build the ggplot facet map of selected intervention groups
#'
#' @description Takes the long-format district x intervention-group data
#'   produced by `prepare_facet_data()` and renders a small-multiple map with
#'   one facet per intervention group, coloured by intervention status.
#'
#' @param plot_dt_long data.table. Long-format output from
#'   `prepare_facet_data()`, with columns: admin_2, Group, Status.
#' @param shape_file_tza sf object. Tanzania district shapefile.
#' @param tza_outline sf object. Tanzania national outline (for background).
#'
#' @return A `ggplot` object.
#'
#' @export
make_facet_map <- function(plot_dt_long, shape_file_tza, tza_outline) {
  # Colour palette: 14 distinct statuses across 7 intervention groups
  status_colors <- c(
    # CM & ICCM
    "CM Only" = "#2196F3",
    "iCCM Only" = "#00796B",
    "CM + iCCM" = "#1A237E",
    # PMC & SMC
    "PMC Only" = "#7B1FA2",
    "SMC Only" = "#388E3C",
    "PMC + SMC" = "#4A148C",
    # Nets
    "STD Nets" = "#90CAF9",
    "PBO Nets" = "#1565C0",
    "IG2 Nets" = "#0D47A1",
    "Multiple Nets" = "#01579B",
    # Single interventions
    "LSM" = "#F57F17",
    "Vaccine" = "#C62828",
    "IPTSc" = "#558B2F",
    "IRS" = "#6D4C41"
  )

  map_obj <- build_map_obj(plot_dt_long, shape_file_tza)
  # Remove unmatched shapefile rows (districts with no active interventions)
  map_obj <- map_obj[!is.na(map_obj$Group), ]

  ggplot2$ggplot(map_obj) +
    ggplot2$geom_sf(
      data = tza_outline,
      fill = "#f2f2f2",
      color = "grey50",
      linewidth = 0.05
    ) +
    ggplot2$geom_sf(
      ggplot2$aes(fill = Status),
      color = "black",
      linewidth = 0.1
    ) +
    ggplot2$facet_wrap(~Group, ncol = 3, drop = TRUE) +
    ggplot2$scale_fill_manual(
      values = status_colors,
      name = "Intervention Status",
      drop = FALSE
    ) +
    ggplot2$theme_void() +
    ggplot2$theme(
      strip.text = ggplot2$element_text(
        size = 13,
        face = "bold",
        margin = ggplot2$margin(b = 10)
      ),
      legend.position = "bottom",
      legend.title = ggplot2$element_text(face = "bold"),
      panel.spacing = ggplot2$unit(2, "lines"),
      plot.margin = ggplot2$margin(20, 20, 20, 20)
    ) +
    ggplot2$guides(
      fill = ggplot2$guide_legend(
        nrow = 3,
        byrow = TRUE,
        override.aes = list(size = 5)
      )
    )
}


# SENSITIVITY ANALYSIS (run_sensitivity function)

#' Run budget sensitivity analysis across a fixed set of budget steps
#'
#' @description For each budget step in `sens_steps` that is less than or equal
#'   to `max_adj`, re-solves the LP at that budget level and records the total
#'   cases averted. A monotonicity correction is applied: if increasing the
#'   budget yields fewer cases averted (due to LP tie-breaking), the previous
#'   step's value is retained.
#'
#' @param res_mean data.table. EIR_mean-filtered metrics from
#'   `compute_metrics()`.
#' @param base_budget Numeric. Reference budget (USD), i.e.
#'   `budget_metrics$curr`.
#' @param max_adj Numeric. Maximum slider value (e.g. 20 means show steps up to
#'   +20%).
#' @param health_name Character. Column to maximise in the LP — either
#'   `"averted"` (max health impact) or `"NMB"` (most cost-effective).
#' @param sens_steps Numeric vector. Budget increment percentages to evaluate
#'   (default `c(0, 5, 10, 15, 20, 25, 30)`).
#'
#' @return List of lists, one per active step, each containing:
#'   * `pct`: Numeric. Budget increment percentage.
#'   * `budget`: Numeric. Absolute budget at this step (USD).
#'   * `cases_averted`: Numeric. Total cases averted vs reference at this
#'     budget.
#'   * `opt_res`: data.table or NULL. LP result at this step.
#'
#'   Attributes on the returned list:
#'   * `actual_cost`: Numeric. Actual cost of the optimal plan at baseline (0%).
#'   * `actual_cost_pct`: Numeric. Percentage difference vs base_budget.
#'   * `base_budget`: Numeric. The base_budget passed in.
#'   * `health_name`: Character. The health_name used.
#'
#' @export
run_sensitivity <- function(
  res_mean,
  base_budget,
  max_adj,
  health_name = "averted",
  sens_steps = seq(-10, 10, by = 2)
) {
  # Show all steps from the most negative up to current slider value
  # Always include 0% as the baseline reference point
  active_steps <- sort(unique(c(0L, sens_steps[sens_steps <= max_adj])))

  results <- lapply(active_steps, function(pct) {
    bud <- base_budget * (1 + pct / 100)

    # Guard: budget must be positive
    if (bud <= 0) {
      return(list(pct = pct, budget = bud, cases_averted = 0, opt_res = NULL))
    }

    opt <- tryCatch(
      optimal_allocation(
        df = res_mean,
        budget_env = bud,
        region_name = "admin_2",
        policy_name = "scenario_name",
        cost_name = "avg_cost",
        health_name = health_name,
        optim_dir = "max"
      ),
      error = function(e) NULL
    )

    if (is.null(opt) || nrow(opt) == 0L) {
      list(pct = pct, budget = bud, cases_averted = 0, opt_res = NULL)
    } else {
      list(
        pct = pct,
        budget = bud,
        cases_averted = sum(opt$averted, na.rm = TRUE),
        opt_res = opt
      )
    }
  })

  #  Monotonicity(orderly increaments) corrections
  # Get baseline (0%) cases averted
  baseline_idx <- which(sapply(results, `[[`, "pct") == 0)
  baseline <- results[[baseline_idx]]$cases_averted

  # Positive steps: cases averted must be non-decreasing as budget increases
  pos_idx <- which(sapply(results, `[[`, "pct") > 0)
  if (length(pos_idx) > 0L) {
    prev <- baseline
    for (i in pos_idx) {
      if (results[[i]]$cases_averted < prev) {
        results[[i]]$cases_averted <- prev
      }
      prev <- results[[i]]$cases_averted
    }
  }

  # Negative steps: cases averted must be non-increasing as budget decreases
  # i.e. cutting budget cannot increase cases averted vs baseline
  neg_idx <- rev(which(sapply(results, `[[`, "pct") < 0)) # from -2 down to most negative
  if (length(neg_idx) > 0L) {
    prev <- baseline
    for (i in neg_idx) {
      if (results[[i]]$cases_averted > prev) {
        results[[i]]$cases_averted <- prev
      }
      prev <- results[[i]]$cases_averted
    }
  }

  # Attach actual cost of the baseline optimal allocation so the chart can
  # show the efficiency frontier and savings annotation.
  baseline_opt <- results[[baseline_idx]]$opt_res
  if (!is.null(baseline_opt) && nrow(baseline_opt) > 0L) {
    actual <- sum(baseline_opt$avg_cost, na.rm = TRUE)
    attr(results, "actual_cost") <- actual
    attr(results, "actual_cost_pct") <- round(
      (actual / base_budget - 1) * 100,
      1
    )
  } else {
    attr(results, "actual_cost") <- base_budget
    attr(results, "actual_cost_pct") <- 0
  }
  attr(results, "base_budget") <- base_budget
  attr(results, "health_name") <- health_name

  results
}
