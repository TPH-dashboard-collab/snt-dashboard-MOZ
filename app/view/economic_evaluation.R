# fmt: skip
box::use(
  bslib,
  config,
  data.table[
    `:=`, `.N`, `.SD`, `%chin%`,
    as.data.table, data.table, fifelse, fcase, merge.data.table, setorder, fwrite
  ],
  ggplot2[annotate, ggplot, theme_void],
  htmltools[HTML],
  leaflet[
    addLegend, addPolygons, addProviderTiles, colorFactor,
    highlightOptions, leaflet, leafletOutput, providers, renderLeaflet,
  ],
  logger[log_debug, log_info],
  plotly[
    event_data, event_register, layout, plot_ly, plotly_empty,
    plotlyOutput, renderPlotly,
  ],
  rlang[`%||%`],
  sf[st_as_sf, st_make_valid, st_union],
  shiny,
)

# fmt: skip
box::use(
  app/logic/analytics/optimal_allocation[
    build_map_obj,
    classify_district,
    compute_budget,
    compute_incremental_metrics,
    compute_metrics,
    make_facet_map,
    make_tornado,
    optimal_allocation,
    prepare_facet_data,
    run_sensitivity,
  ],
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/core/utils[nvec_get_val],
  app/logic/data/get_filtered_data[add_intervention_combo_column],
  app/logic/visualization/formatters,
)

# =============================================================================
# Economic Evaluation (EE) Module
# =============================================================================
#
# PURPOSE
# -------
# Answers the question: "Given a budget, what is the optimal intervention mix
# across all districts?"  It compares every scenario from the simulation DB
# against a user-chosen reference plan (BAU, NSP, or a Custom scenario from
# the scenario customizer) and runs a linear programme (lpSolve) to find
# district-level allocations that maximise health impact or Net Monetary
# Benefit (NMB) within a budget envelope.
#
# DATA FLOW  (server-side reactive chain)
# ----------------------------------------
#   1. ee_data          – Pre-aggregated district summaries fetched from DB
#                         (one row per scenario × district × EIR_CI; no year
#                         dimension). Contains vol_*, cum_cases_*, deployed_int_*.
#
#   2. metrics_data     – Adds avg_cost (weighted by unit costs) and
#                         averted_period (cases over the plan period) to each
#                         row. No reference comparison yet.
#
#   3. ref_data         – The reference baseline the user selected:
#                         • BAU / NSP → filtered subset of metrics_data
#                         • Custom    → aggregated in R from the scenario
#                           customizer's output (variables$session_state$custom_data)
#                         Columns: admin_2, EIR_CI, avg_cost, averted_period
#
#   4. incremental_data – Merges metrics_data with ref_data to compute per-row:
#                         averted, cost_diff, NMB, ICER, is_CE
#
#   5. budget_metrics   – Derives the budget envelope from ref_data's total
#                         cost + the user's % adjustment slider.
#
#   6. res_mean_dedup   – Filters incremental_data to EIR_mean and deduplicates
#                         by (admin_2, scenario_name) → the LP input.
#
#   7. opt_res_NMB /    – Two LP solves: one maximising NMB, one maximising
#      opt_res_averted    cases averted, both subject to the budget envelope.
#
# UI TABS
# -------
#   • Overview          – Value boxes, two optimiser maps (NMB vs Health),
#                         tornado charts, facet intervention map.
#   • Budget Sensitivity– Bar chart showing cases averted at each budget step;
#                         click a bar to render the corresponding facet map.
#   • Budget Planner    – User enters a custom budget, runs a one-off LP,
#                         sees the resulting allocation map + value boxes.
#
# KEY DEPENDENCIES (app/logic/analytics/optimal_allocation.R)
#   compute_metrics, compute_incremental_metrics, compute_budget,
#   optimal_allocation, run_sensitivity, prepare_facet_data, make_facet_map,
#   make_tornado, build_map_obj, build_scen_int_counts, classify_district
# =============================================================================

# Budget sensitivity steps — asymmetric:
# Negative side: steps of 5; positive side: steps of 2
SENS_STEPS <- c(seq(-30, -5, by = 5), seq(0, 10, by = 2))

# UI ---------------------------------------------------------------------------

#' @export
ui <- function(id, use_custom_data = FALSE) {
  ns <- shiny$NS(id)

  plan_choices <- names(unlist(unname(config$get("plans"))))
  if (use_custom_data) {
    plan_choices <- c(plan_choices, "Custom")
  }

  shiny$tagList(
    # EE-specific controls
    bslib$card(
      bslib$card_header("Economic Evaluation Settings"),
      bslib$card_body(
        shiny$fluidRow(
          shiny$column(
            4,
            shiny$numericInput(
              ns("wtp"),
              "Willingness to Pay (per case averted, USD)",
              value = 5,
              min = 0
            )
          ),
          shiny$column(
            4,
            shiny$sliderInput(
              ns("budget_adj"),
              "Budget Change (%)",
              min = -30,
              max = 10,
              value = 0
            )
          ),
          shiny$column(
            4,
            shiny$selectInput(
              ns("reference_plan"),
              "Reference Plan",
              choices = plan_choices,
              selected = plan_choices[[1]]
            )
          )
        )
      )
    ),

    # Main content tabs
    bslib$navset_card_pill(
      id = ns("ee_tabs"),
      title = "Budget Optimization",

      ## Overview tab ----------------------------------------------------------
      bslib$nav_panel(
        "Overview",

        # Value boxes
        shiny$uiOutput(ns("overview_value_boxes")),

        # Maps: NMB + Health optimizer
        shiny$fluidRow(
          shiny$column(
            6,
            bslib$card(
              bslib$card_header("1) Most Cost-Effective Plan per District"),
              bslib$card_body(
                shiny$p(
                  "Shows the optimal intervention combination per district ",
                  "that maximises Net Monetary Benefit (NMB).",
                  style = "color: #666; font-style: italic;"
                ),
                map_legend_html(),
                leafletOutput(ns("map_ce"), height = "480px")
              )
            )
          ),
          shiny$column(
            6,
            bslib$card(
              bslib$card_header("2) Optimal Plan for Maximum Health Impact"),
              bslib$card_body(
                shiny$p(
                  "Identifies which intervention combination maximises total ",
                  "cases averted per district within the current budget.",
                  style = "color: #666; font-style: italic;"
                ),
                map_legend_html(),
                leafletOutput(ns("map_opt_assess"), height = "480px")
              )
            )
          )
        ),

        # Tornado charts
        bslib$card(
          bslib$card_header(
            "Cost vs Cases Averted \u2014 Optimal Allocations per District"
          ),
          bslib$card_body(
            shiny$p(
              "Each row shows one district's optimizer-selected intervention ",
              "combination. Left side (gold) = total cost in USD. Right side ",
              "(purple) = cases averted vs reference.",
              style = "color: #666; font-style: italic;"
            ),
            shiny$fluidRow(
              shiny$column(
                6,
                shiny$p(
                  shiny$strong("Map 1 \u2014 NMB Optimizer"),
                  style = "text-align:center;"
                ),
                shiny$plotOutput(ns("tornado_nmb"), height = "420px")
              ),
              shiny$column(
                6,
                shiny$p(
                  shiny$strong("Map 2 \u2014 Health Optimizer"),
                  style = "text-align:center;"
                ),
                shiny$plotOutput(ns("tornado_averted"), height = "420px")
              )
            )
          )
        ),

        # Facet map
        bslib$card(
          bslib$card_header("3) Optimal Allocation at Current Budget"),
          bslib$card_body(
            shiny$p(
              "Reveals the strategic national footprint of intervention groups ",
              "optimized for the current budget envelope.",
              style = "color: #666; font-weight: bold;"
            ),
            shiny$plotOutput(ns("map_facets"), height = "800px")
          )
        )
      ),

      ## Budget Sensitivity tab ------------------------------------------------
      bslib$nav_panel(
        "Budget Sensitivity",

        bslib$card(
          bslib$card_header(
            "Budget Sensitivity \u2014 Cases Averted vs Reference"
          ),
          bslib$card_body(
            shiny$p(
              shiny$tags$i(class = "bi bi-info-circle"),
              "Adjust the ",
              shiny$strong("Budget Change (%)"),
              " slider above. Bars appear at every ",
              shiny$strong("2% interval"),
              " from -30% to +10%. Red bars show budget cuts, gold is ",
              "baseline (0%), blue is increases. The ",
              shiny$strong("green dashed line"),
              " marks the efficiency frontier. ",
              shiny$strong("Click any bar"),
              " to update the intervention map below.",
              style = "color: #555; margin-bottom: 10px;"
            ),
            plotlyOutput(ns("sensitivity_bar"), height = "450px"),
            shiny$hr(),
            shiny$uiOutput(ns("sensitivity_budget_label")),
            shiny$plotOutput(ns("sensitivity_facet"), height = "800px")
          )
        )
      ),

      ## Budget Planner tab ----------------------------------------------------
      bslib$nav_panel(
        "Budget Planner",

        shiny$fluidRow(
          shiny$column(
            4,
            bslib$card(
              bslib$card_header("Budget Simulation Settings"),
              bslib$card_body(
                shiny$p(
                  "Enter a total national budget to find the optimal ",
                  "intervention mix across all districts."
                ),
                shiny$uiOutput(ns("planner_ref_budget_hint")),
                shiny$numericInput(
                  ns("user_budget_amount"),
                  "Total National Budget (USD):",
                  value = 67810335,
                  min = 0,
                  step = 1000000
                ),
                bslib$input_task_button(
                  id = ns("run_planner"),
                  label = "Run Optimization"
                )
              )
            )
          ),
          shiny$column(8, shiny$uiOutput(ns("planner_value_boxes")))
        ),

        bslib$card(
          bslib$card_header("Custom Strategic Allocation Map"),
          bslib$card_body(
            shiny$p(
              "This map shows the optimal intervention combination per ",
              "district to maximise health within your entered budget."
            ),
            map_legend_html(),
            leafletOutput(ns("map_planner"), height = "580px")
          )
        )
      )
    )
  )
}


# Server -----------------------------------------------------------------------

#' @export
server <- function(
  id,
  variables,
  country_map,
  use_custom_data = FALSE,
  trigger,
  is_visible = shiny$reactiveVal(TRUE),
  ...
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    args <- list(...)
    log_ns <- log_ns_builder(args, "economic_evaluation")

    # ── Map geometry ──────────────────────────────────────────────────────────
    # Convert the shared country_map (data.frame/sf) into an sf object with a
    # join_id column used by build_map_obj() to match district names.

    country_map_sf <- shiny$reactive({
      cm <- as.data.table(country_map)
      cm[, join_id := toupper(trimws(as.character(admin_2)))]
      cm[, JOIN_TARGET := admin_2]
      st_as_sf(cm)
    })

    country_outline <- shiny$reactive({
      st_make_valid(country_map_sf())
    })

    # ── Step 1: Data fetching ─────────────────────────────────────────────────
    # Fetches pre-aggregated district summaries from the DB via
    # get_ee_district_summaries(). One row per (scenario_name, admin_2, EIR_CI).
    # Columns include vol_<int> (annual avg intervention volume), cum_cases_start,
    # cum_cases_end, and deployed_int_<int> flags.
    # This is the ONLY DB fetch in the EE module — all downstream reactives
    # derive from this.
    # REVIEW: This might be a good place to run async.
    # Track whether the section has been opened at least once.
    has_been_visible <- shiny$reactiveVal(FALSE)
    shiny$observe({
      if (is_visible()) has_been_visible(TRUE)
    })

    ee_data <- shiny$reactive({
      # Don't fetch until the section is opened for the first time.
      # Once activated, toggling off/on won't re-trigger.
      shiny$req(has_been_visible())

      log_debug(
        "Fetching EE data...",
        namespace = log_ns_fn(log_ns)
      )

      shiny$req(age_group_d())

      filters <- list()
      if (
        variables$session_state$agg_level == "Regional" &&
          length(variables$session_state$region_selected) > 0
      ) {
        filters$admin_1 <- variables$session_state$region_selected
      }

      dt <- variables$data_con$get_ee_district_summaries(
        intervention_cols = variables$intervention_cols,
        age_groups = age_group_d(),
        year_start = variables$session_state$year_start,
        year_end = variables$session_state$year_end,
        filters = filters,
        log_ns = log_ns
      )
      if (!"intervention_combo" %chin% colnames(dt)) {
        dt <- add_intervention_combo_column(dt)
      }

      log_info(
        "EE data is {nrow(dt)} rows",
        namespace = log_ns_fn(log_ns)
      )
      dt
    })

    # ── Lookup tables ─────────────────────────────────────────────────────────
    # int_names:       Short intervention names (e.g. "CM", "IRS") stripped of
    #                  the "deployed_int_" prefix.
    # scen_lookup:     Unique (admin_2, scenario_name, intervention_combo) for
    #                  tooltip labels and map annotations.
    # scen_int_counts: Per-scenario count of active interventions, used by
    #                  classify_district() to label districts as "Added",
    #                  "Reduced", "Substituted", or "Reference".

    int_names <- shiny$reactive({
      gsub("^deployed_int_", "", variables$intervention_cols)
    })

    scen_lookup <- shiny$reactive({
      shiny$req(ee_data())
      unique(
        ee_data()[, c("admin_2", "scenario_name", "intervention_combo")],
        by = c("admin_2", "scenario_name")
      )
    })

    ## Debounced EE-specific inputs --------------------------------------------

    wtp_d <- shiny$debounce(shiny$reactive(input$wtp), 800)
    budget_adj_d <- shiny$debounce(shiny$reactive(input$budget_adj), 800)
    age_group_d <- shiny$debounce(
      shiny$reactive(variables$session_state$age_group),
      800
    )

    # ── Step 2: Base metrics ──────────────────────────────────────────────────
    # Adds avg_cost (sum of vol_<int> * unit_cost per intervention) and
    # averted_period (cum_cases_end - cum_cases_start) to each row.
    # Does NOT depend on reference plan or WTP — only re-runs when ee_data
    # or unit costs change.
    metrics_data <- shiny$reactive({
      shiny$req(ee_data(), int_names())

      u_costs <- variables$session_state$unit_costs
      shiny$req(u_costs)

      log_debug("Computing EE metrics...", namespace = log_ns_fn(log_ns))

      compute_metrics(
        data = ee_data(),
        int_names = int_names(),
        u_costs = u_costs
      )
    })

    # ── Step 3: Reference baseline ─────────────────────────────────────────────
    # reference_scenario() maps the UI label ("BAU", "NSP", "Custom") to the
    # internal scenario key ("bau", "nsp", "custom").
    #
    # ref_data() returns the reference baseline as a table with columns:
    #   (admin_2, EIR_CI, avg_cost, averted_period)
    #
    # • BAU / NSP: simple filter of metrics_data — these scenarios already
    #   exist in the DB.
    # • Custom: the scenario customizer's output (raw simulation rows) is
    #   aggregated in R to match the same shape. This is the only place
    #   where custom_data is consumed in the EE module.
    #
    # REVIEW: The Custom aggregation block should be extracted into a function.

    reference_scenario <- shiny$reactive({
      if (input$reference_plan == "Custom") {
        "custom"
      } else {
        nvec_get_val(unlist(unname(config::get("plans"))), input$reference_plan)
      }
    })

    ref_data <- shiny$reactive({
      shiny$req(metrics_data(), reference_scenario())

      if (reference_scenario() == "custom") {
        shiny$req(variables$session_state$custom_data)
        # Custom data also includes the reference/counterfactual data. Exclude
        # that here.
        cd <- variables$session_state$custom_data[scenario_name == "custom"]

        u_costs <- variables$session_state$unit_costs
        shiny$req(u_costs)

        year_start <- variables$session_state$year_start
        year_end <- variables$session_state$year_end
        n_years <- year_end - year_start + 1L

        # Costs: same logic as DB query — SUM(nHost * deployed * coverage)
        # across year range, divided by n_years, then multiply by unit cost
        dt <- cd[year >= year_start & year <= year_end]
        dt[, avg_cost := 0]
        for (int in int_names()) {
          dep <- paste0("deployed_int_", int)
          cov <- paste0("coverage_int_", int)
          if (dep %in% names(dt) && cov %in% names(dt)) {
            dt[,
              avg_cost := avg_cost +
                (nHost * get(dep) * get(cov) * u_costs[[int]])
            ]
          }
        }
        costs <- dt[,
          .(avg_cost = sum(avg_cost) / n_years),
          by = .(admin_2, EIR_CI)
        ]

        # Cases: AVG(cum_nUncomp) at year_end minus at year_start - 1
        cases_end <- cd[
          year == year_end,
          .(c_e = mean(cum_nUncomp)),
          by = .(admin_2, EIR_CI)
        ]
        cases_start <- cd[
          year == (year_start - 1L),
          .(c_s = mean(cum_nUncomp)),
          by = .(admin_2, EIR_CI)
        ]
        impact <- merge(
          cases_end,
          cases_start,
          by = c("admin_2", "EIR_CI"),
          all.x = TRUE
        )
        impact[, averted_period := c_e - fifelse(is.na(c_s), 0, c_s)]

        return(merge(
          costs,
          impact[, .(admin_2, EIR_CI, averted_period)],
          by = c("admin_2", "EIR_CI")
        ))
      }

      metrics_data()[
        scenario_name == reference_scenario(),
        .(admin_2, EIR_CI, avg_cost, averted_period)
      ]
    })

    # Intervention-combo lookup for the reference plan (admin_2 →
    # intervention_combo). For BAU/NSP this comes from scen_lookup(); for
    # Custom it is derived from custom_data's deployed_int_* columns.
    ref_combo_lookup <- shiny$reactive({
      shiny$req(reference_scenario())

      if (reference_scenario() != "custom") {
        return(
          scen_lookup()[
            scenario_name == reference_scenario(),
            .(admin_2, intervention_combo)
          ]
        )
      }

      shiny$req(variables$session_state$custom_data)
      cd <- variables$session_state$custom_data[scenario_name == "custom"]
      cd <- unique(cd, by = "admin_2")
      if (!"intervention_combo" %chin% colnames(cd)) {
        cd <- add_intervention_combo_column(cd)
      }
      unique(cd[, .(admin_2, intervention_combo)])
    })

    # ── Step 4: Incremental comparison ────────────────────────────────────────
    # Merges each scenario row with ref_data by (admin_2, EIR_CI) to compute:
    #   averted   = ref cases − scenario cases  (positive = scenario is better)
    #   cost_diff = scenario cost − ref cost
    #   NMB       = (averted × WTP) − cost_diff
    #   ICER      = cost_diff / averted
    #   is_CE     = NMB >= 0
    # Re-runs when reference plan or WTP changes.
    incremental_data <- shiny$reactive({
      shiny$req(metrics_data(), ref_data(), wtp_d())
      compute_incremental_metrics(
        metrics = metrics_data(),
        ref = ref_data(),
        wtp = wtp_d()
      )
    })

    # ── Step 5: Budget envelope ────────────────────────────────────────────────
    # Total cost of the selected reference plan + user's % adjustment.
    # Returns list(curr = base cost, env = adjusted envelope, adj_pct).
    budget_metrics <- shiny$reactive({
      shiny$req(ref_data(), budget_adj_d())
      base_cost <- ref_data()[, sum(avg_cost, na.rm = TRUE)]
      compute_budget(
        base_cost = base_cost,
        budget_adj = budget_adj_d()
      )
    })

    # ── Step 6: LP input ──────────────────────────────────────────────────────
    # Filter to EIR_mean (point estimate, not confidence bounds) and deduplicate
    # to one row per (admin_2, scenario_name). This is the input table for the
    # linear programme — each row is a candidate "policy" for a "region".
    res_mean_dedup <- shiny$reactive({
      shiny$req(incremental_data())
      incremental_data()[
        EIR_CI == "EIR_mean",
        .SD[1],
        by = .(admin_2, scenario_name)
      ]
    })

    # ── Step 7: LP solves ─────────────────────────────────────────────────────
    # Two independent optimisations over the same data and budget:
    #   opt_res_NMB     – maximise total Net Monetary Benefit
    #   opt_res_averted – maximise total cases averted
    # Each returns one row per district (the chosen scenario).
    opt_res_NMB <- shiny$reactive({
      shiny$req(budget_metrics(), res_mean_dedup())
      optimal_allocation(
        df = res_mean_dedup(),
        budget_env = budget_metrics()$env,
        region_name = "admin_2",
        policy_name = "scenario_name",
        cost_name = "avg_cost",
        health_name = "NMB",
        optim_dir = "max"
      )
    })

    opt_res_averted <- shiny$reactive({
      shiny$req(budget_metrics(), res_mean_dedup())
      optimal_allocation(
        df = res_mean_dedup(),
        budget_env = budget_metrics()$env,
        region_name = "admin_2",
        policy_name = "scenario_name",
        cost_name = "avg_cost",
        health_name = "averted",
        optim_dir = "max"
      )
    })

    # Facet map reuses the health-optimised result
    opt_res_facet <- opt_res_averted

    # ── Budget Sensitivity ────────────────────────────────────────────────────
    # Re-runs the LP at each budget step in SENS_STEPS (e.g. -30% to +10%)
    # to show how cases averted change with budget. Results drive the
    # interactive bar chart on the "Budget Sensitivity" tab.
    sensitivity_results <- shiny$reactive({
      shiny$req(budget_metrics(), res_mean_dedup())
      run_sensitivity(
        res_mean = res_mean_dedup(),
        base_budget = budget_metrics()$curr,
        max_adj = budget_adj_d(),
        health_name = "averted",
        sens_steps = SENS_STEPS
      )
    })

    # ── Budget Planner ────────────────────────────────────────────────────────
    # One-shot LP triggered by the "Run Optimization" button. The user enters
    # a custom budget amount and the planner finds the best allocation.
    # Rows with negative averted are excluded (scenarios worse than reference).
    # If the budget is too low for any allocation, falls back to the reference
    # plan for every district and attaches a warning message.
    planner_results <- shiny$eventReactive(input$run_planner, {
      shiny$req(incremental_data(), res_mean_dedup())

      res_mean <- res_mean_dedup()[averted >= 0]
      entered_budget <- input$user_budget_amount
      ref_info <- ref_combo_lookup()
      all_districts <- data.table(admin_2 = unique(ee_data()$admin_2))

      opt_res <- optimal_allocation(
        df = res_mean,
        budget_env = entered_budget,
        region_name = "admin_2",
        policy_name = "scenario_name",
        cost_name = "avg_cost",
        health_name = "averted",
        optim_dir = "max"
      )

      if (nrow(opt_res) == 0L) {
        final_choices <- res_mean[
          scenario_name == reference_scenario(),
          .(admin_2, scenario_name, cost_val = avg_cost, health_val = 0)
        ]
        warning_msg <- paste0(
          "Budget of $",
          format(round(entered_budget), big.mark = ","),
          " is below minimum required. Showing reference plan."
        )
      } else {
        final_choices <- opt_res[, .(
          admin_2 = region,
          scenario_name = policy,
          cost_val = avg_cost,
          health_val = averted
        )]
        warning_msg <- NULL
      }

      comparison <- merge(
        all_districts,
        final_choices,
        by = "admin_2",
        all.x = TRUE
      )
      comparison <- merge(
        comparison,
        scen_lookup(),
        by = c("admin_2", "scenario_name"),
        all.x = TRUE
      )
      comparison <- merge(
        comparison,
        ref_info[, .(admin_2, ref_summary = intervention_combo)],
        by = "admin_2",
        all.x = TRUE
      )

      comparison[,
        final_tools := fifelse(
          is.na(intervention_combo),
          ref_summary,
          intervention_combo
        )
      ]

      comparison[,
        final_plan := mapply(
          classify_district,
          opt_combo = final_tools,
          ref_combo = ref_summary
        )
      ]

      attr(comparison, "entered_budget") <- entered_budget
      attr(comparison, "warning_msg") <- warning_msg
      comparison
    })

    # ── Shared helpers ────────────────────────────────────────────────────────

    # Pick the single best allocation per district from LP output.
    # REVIEW: Move all into dedicated files
    get_best_allocation <- function(opt_res) {
      opt_res[
        policy_allocation > 0,
        .SD[which.max(policy_allocation)],
        by = region
      ]
    }

    # Build a Leaflet choropleth from an LP result. Used by both the NMB and
    # Health optimizer maps on the Overview tab. Each district is coloured by
    # how its optimal allocation compares to the reference (Added / Reduced /
    # Substituted / Reference). Tooltips differ by maximize_goal.
    render_optimized_leaflet <- function(maximize_goal) {
      shiny$req(budget_metrics())

      opt_res <- if (maximize_goal == "NMB") {
        opt_res_NMB()
      } else {
        opt_res_averted()
      }
      shiny$req(nrow(opt_res) > 0)
      opt_choices <- get_best_allocation(opt_res)

      # Merge intervention summaries
      opt_choices <- merge(
        opt_choices[, .(admin_2 = region, scenario_name = policy)],
        scen_lookup(),
        by = c("admin_2", "scenario_name"),
        all.x = TRUE
      )

      all_districts <- data.table(admin_2 = unique(ee_data()$admin_2))
      comparison <- merge(
        all_districts,
        opt_choices,
        by = "admin_2",
        all.x = TRUE
      )

      ref_info <- ref_combo_lookup()
      comparison <- merge(
        comparison,
        ref_info[, .(admin_2, ref_summary = intervention_combo)],
        by = "admin_2",
        all.x = TRUE
      )

      comparison[,
        final_tools := fifelse(
          is.na(intervention_combo),
          ref_summary,
          intervention_combo
        )
      ]

      comparison[,
        final_plan := mapply(
          classify_district,
          opt_combo = final_tools,
          ref_combo = ref_summary
        )
      ]

      # Merge metric values for tooltip
      opt_metrics <- opt_res[, .(
        admin_2 = region,
        scenario_name = policy,
        NMB = NMB,
        averted = averted,
        avg_cost = avg_cost,
        ICER = ICER,
        is_CE = is_CE
      )]
      comparison <- merge(
        comparison,
        opt_metrics,
        by = c("admin_2", "scenario_name"),
        all.x = TRUE
      )

      # Build tooltips
      if (maximize_goal == "NMB") {
        map_obj <- build_map_obj(comparison, country_map_sf())
        map_obj$disp_label <- lapply(
          paste0(
            "<b>",
            map_obj$JOIN_TARGET,
            "</b><br>",
            "<b>Status:</b> ",
            map_obj$final_plan,
            "<br>",
            "<hr style='margin:4px 0;'>",
            "<b>Reference (",
            input$reference_plan,
            "):</b> ",
            map_obj$ref_summary,
            "<br>",
            "<b>Optimal Selected:</b> ",
            map_obj$final_tools,
            "<br>",
            "<hr style='margin:4px 0;'>",
            "<b>NMB:</b> $",
            ifelse(
              is.na(map_obj$NMB),
              "N/A",
              format(round(map_obj$NMB), big.mark = ",")
            ),
            "<br>",
            "<b>Cost-Effective:</b> ",
            ifelse(
              is.na(map_obj$is_CE),
              "N/A",
              ifelse(map_obj$is_CE, "Yes \u2713", "No \u2717")
            ),
            "<br>",
            "<b>ICER:</b> ",
            ifelse(
              is.na(map_obj$ICER),
              "N/A",
              paste0("$", format(round(map_obj$ICER, 2), big.mark = ","))
            )
          ),
          HTML
        )
      } else {
        map_obj <- build_map_obj(comparison, country_map_sf())
        map_obj$disp_label <- lapply(
          paste0(
            "<b>",
            map_obj$JOIN_TARGET,
            "</b><br>",
            "<b>Status:</b> ",
            map_obj$final_plan,
            "<br>",
            "<hr style='margin:4px 0;'>",
            "<b>Reference (",
            input$reference_plan,
            "):</b> ",
            map_obj$ref_summary,
            "<br>",
            "<b>Optimal Selected:</b> ",
            map_obj$final_tools,
            "<br>",
            "<hr style='margin:4px 0;'>",
            "<b>Cases Averted vs Reference:</b> ",
            ifelse(
              is.na(map_obj$averted),
              "N/A",
              format(round(map_obj$averted), big.mark = ",")
            ),
            "<br>",
            "<b>Plan Cost:</b> $",
            ifelse(
              is.na(map_obj$avg_cost),
              "N/A",
              format(round(map_obj$avg_cost), big.mark = ",")
            )
          ),
          HTML
        )
      }

      # Colour palette
      all_labels <- c("Reference", "Added", "Reduced", "Substituted")
      pal <- colorFactor(
        palette = c("#756bb1", "#2ca25f", "#e67e22", "#2980b9"),
        levels = all_labels
      )

      leaflet(map_obj) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
          fillColor = ~ pal(final_plan),
          weight = 1,
          color = "white",
          fillOpacity = 0.8,
          label = ~disp_label,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          pal = pal,
          values = all_labels,
          title = HTML(
            "Optimal vs Reference<br>",
            "<small style='font-weight:normal;font-style:italic;'>",
            "Hover district for interventions</small>"
          )
        )
    }

    # ==========================================================================
    # OUTPUT RENDERERS
    # ==========================================================================

    # ── Overview tab: value boxes ─────────────────────────────────────────────
    # Four KPI boxes: reference budget, active budget envelope, reference
    # total cases, and optimised total cases with % reduction.
    output$overview_value_boxes <- shiny$renderUI({
      shiny$req(budget_metrics(), opt_res_averted(), metrics_data())

      adj <- budget_metrics()$adj_pct
      sign <- ifelse(adj >= 0, "+", "")

      ref_total <- ref_data()[
        EIR_CI == "EIR_mean",
        sum(averted_period, na.rm = TRUE)
      ]
      opt <- opt_res_averted()
      cases_averted <- sum(opt$averted, na.rm = TRUE)
      cases_opt <- ref_total - cases_averted
      pct_red <- if (ref_total != 0) round(cases_averted / ref_total * 100, 1) else 0

      shiny$fluidRow(
        shiny$column(
          3,
          bslib$value_box(
            title = "Reference Budget Envelope",
            value = paste0(
              formatters$format_number(round(budget_metrics()$curr)),
              " $"
            ),
            theme = "primary"
          )
        ),
        shiny$column(
          3,
          bslib$value_box(
            title = paste0("Active Budget Envelope (", sign, adj, "%)"),
            value = paste0(
              formatters$format_number(round(budget_metrics()$env)),
              " $"
            ),
            theme = "secondary"
          )
        ),
        shiny$column(
          3,
          bslib$value_box(
            title = paste0("Cases Reference (", input$reference_plan, ")"),
            value = formatters$format_number(round(ref_total)),
            theme = "primary"
          )
        ),
        shiny$column(
          3,
          bslib$value_box(
            title = paste0(
              "Cases Optimal (",
              formatters$format_number(round(cases_averted)),
              " averted, ",
              pct_red,
              "%)"
            ),
            value = formatters$format_number(round(cases_opt)),
            theme = "success"
          )
        )
      )
    })

    # ── Overview tab: optimiser maps ──────────────────────────────────────────
    output$map_ce <- renderLeaflet({
      render_optimized_leaflet("NMB")
    })

    output$map_opt_assess <- renderLeaflet({
      render_optimized_leaflet("averted")
    })

    # ── Overview tab: tornado charts ──────────────────────────────────────────
    # Side-by-side tornado plots showing cost (left) vs cases averted (right)
    # per district, one for each optimiser.
    output$tornado_nmb <- shiny$renderPlot({
      shiny$req(opt_res_NMB(), nrow(opt_res_NMB()) > 0)
      make_tornado(opt_res_NMB(), scen_lookup())
    })

    output$tornado_averted <- shiny$renderPlot({
      shiny$req(opt_res_averted(), nrow(opt_res_averted()) > 0)
      make_tornado(opt_res_averted(), scen_lookup())
    })

    # ── Overview tab: facet intervention map ───────────────────────────────────
    # Small-multiples map: one panel per intervention, showing which districts
    # deploy it under the health-optimised allocation.
    output$map_facets <- shiny$renderPlot({
      shiny$req(opt_res_facet(), ee_data())
      shiny$req(nrow(opt_res_facet()) > 0)
      opt_choices <- get_best_allocation(opt_res_facet())
      plot_dt_long <- prepare_facet_data(
        opt_choices,
        ee_data(),
        variables$intervention_cols
      )
      make_facet_map(plot_dt_long, country_map_sf(), country_outline())
    })

    # ── Sensitivity tab: interactive bar chart ─────────────────────────────────
    # Plotly bar chart with one bar per budget step. Colour: red = cut,
    # gold = baseline, blue = increase. A green dashed line marks the
    # efficiency frontier (point where further budget cuts start losing cases).
    # Clicking a bar updates clicked_step, which drives the facet map below.

    clicked_step <- shiny$reactiveVal(0L)

    output$sensitivity_bar <- renderPlotly({
      res <- sensitivity_results()

      if (length(res) == 0L) {
        return(
          plotly_empty() |>
            layout(title = "Move the Budget Change slider to reveal bars")
        )
      }

      actual_cost_pct <- attr(res, "actual_cost_pct")
      actual_cost <- attr(res, "actual_cost")
      base_budget <- attr(res, "base_budget")
      obj_label <- if (identical(attr(res, "health_name"), "NMB")) {
        "Most Cost-Effective"
      } else {
        "Maximum Health Impact"
      }

      df <- data.table(
        pct = sapply(res, `[[`, "pct"),
        budget = sapply(res, `[[`, "budget"),
        cases = sapply(res, `[[`, "cases_averted")
      )
      setorder(df, pct)

      df[, x_label := paste0(ifelse(pct >= 0, "+", ""), pct, "%")]
      df[, budget_lbl := paste0("$", round(budget / 1e6, 1), "M")]
      df[,
        cases_label := ifelse(
          abs(cases) >= 1e6,
          paste0(round(cases / 1e6, 2), "M"),
          ifelse(
            abs(cases) >= 1e3,
            paste0(round(cases / 1e3, 1), "K"),
            format(round(cases), big.mark = ",")
          )
        )
      ]

      df[,
        bar_color := fcase(
          pct < 0L  , "#e74c3c" ,
          pct == 0L , "#f39c12" ,
          default = "#2980b9"
        )
      ]

      df[,
        hover_text := paste0(
          "<b>Budget change: ",
          x_label,
          "</b><br>",
          "Envelope: ",
          budget_lbl,
          "<br>",
          "Cases Averted: ",
          cases_label,
          "<br>",
          "<i>Click to show map below</i>"
        )
      ]

      df[, x_label := factor(x_label, levels = x_label)]

      # Find efficiency frontier
      baseline_cases <- df[pct == 0, cases]
      flat_bars <- df[cases >= baseline_cases * 0.99]
      drop_bars <- df[cases < baseline_cases * 0.99]

      show_frontier <- FALSE
      frontier_x_pos <- 0

      if (nrow(drop_bars) > 0 && nrow(flat_bars) > 0) {
        last_drop_pct <- max(drop_bars$pct)
        first_flat_pct <- min(flat_bars$pct)
        frontier_x_pos <- mean(c(
          which(
            levels(df$x_label) ==
              paste0(ifelse(last_drop_pct >= 0, "+", ""), last_drop_pct, "%")
          ) -
            1,
          which(
            levels(df$x_label) ==
              paste0(
                ifelse(first_flat_pct >= 0, "+", ""),
                first_flat_pct,
                "%"
              )
          ) -
            1
        ))
        show_frontier <- TRUE
      }

      actual_cost_fmt <- paste0("$", round(actual_cost / 1e6, 1), "M")
      savings_fmt <- paste0(
        "$",
        round((base_budget - actual_cost) / 1e6, 1),
        "M"
      )

      # Zoom y-axis: bars always anchor at 0 in plotly, so we need to use
      # a scatter trace to allow a custom y range that highlights variation.
      y_min <- min(df$cases, na.rm = TRUE)
      y_max <- max(df$cases, na.rm = TRUE)
      y_span <- y_max - y_min
      # If all bars are nearly the same, show a meaningful range around them;
      # otherwise pad a little below the minimum.
      if (y_span < y_max * 0.05) {
        y_lo <- y_min - y_max * 0.03
        y_hi <- y_max + y_max * 0.03
      } else {
        y_lo <- max(0, y_min - y_span * 0.15)
        y_hi <- y_max + y_span * 0.15
      }

      p <- plot_ly(
        data = df,
        x = ~x_label,
        y = ~cases,
        type = "bar",
        marker = list(
          color = ~bar_color,
          line = list(color = "white", width = 1.5)
        ),
        text = ~cases_label,
        textposition = "outside",
        hovertext = ~hover_text,
        hoverinfo = "text",
        source = "sens_bar"
      ) |>
        layout(
          title = list(
            text = paste("Cases Averted at Each Budget Level \u2014", obj_label),
            x = 0
          ),
          xaxis = list(
            title = "Budget Step (% change vs reference cost)",
            tickangle = -45,
            categoryorder = "array",
            categoryarray = levels(df$x_label)
          ),
          yaxis = list(
            title = "Cases Averted vs Reference",
            range = list(y_lo, y_hi)
          ),
          showlegend = FALSE,
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          margin = list(t = 120),
          shapes = if (show_frontier) {
            list(list(
              type = "line",
              x0 = frontier_x_pos,
              x1 = frontier_x_pos,
              y0 = 0,
              y1 = 1,
              yref = "paper",
              line = list(color = "#27ae60", dash = "dash", width = 2.5)
            ))
          } else {
            list()
          },
          annotations = if (show_frontier) {
            list(list(
              x = frontier_x_pos,
              y = 0.95,
              yref = "paper",
              xref = "x",
              text = paste0(
                "<b>Efficiency Frontier</b><br>",
                "Optimal plan costs ",
                actual_cost_fmt,
                " (saves ",
                savings_fmt,
                " vs reference)<br>",
                "Same health outcome for less spend.<br>",
                "Cases drop below this line."
              ),
              showarrow = TRUE,
              arrowhead = 2,
              arrowcolor = "#27ae60",
              ax = 80,
              ay = -40,
              font = list(color = "#27ae60", size = 11),
              bgcolor = "rgba(255,255,255,0.9)",
              bordercolor = "#27ae60",
              borderwidth = 1,
              align = "left"
            ))
          } else {
            list()
          }
        )

      event_register(p, "plotly_click")
    })

    # Capture plotly bar clicks → update clicked_step (only on sensitivity tab)
    shiny$observe({
      shiny$req(input$ee_tabs == "Budget Sensitivity")
      click <- event_data("plotly_click", source = "sens_bar")
      shiny$req(click)
      lbl <- as.character(click$x)
      pct <- as.numeric(gsub("[+%]", "", lbl))
      if (!is.na(pct)) clicked_step(pct)
    })

    # ── Sensitivity tab: budget label + efficiency insight ─────────────────────
    output$sensitivity_budget_label <- shiny$renderUI({
      pct <- clicked_step()
      bud <- budget_metrics()$curr * (1 + pct / 100)
      sign <- ifelse(pct >= 0, "+", "")
      actual_cost <- attr(sensitivity_results(), "actual_cost")
      actual_pct <- attr(sensitivity_results(), "actual_cost_pct")
      savings <- budget_metrics()$curr - actual_cost

      shiny$div(
        style = "text-align:center; margin: 6px 0 4px 0;",
        shiny$strong(paste0(
          "Facet map showing: Budget at ",
          sign,
          pct,
          "% = $",
          format(round(bud), big.mark = ",")
        )),
        shiny$br(),
        shiny$p(
          HTML(paste0(
            "<span style='color:#27ae60;font-weight:bold;'>",
            "Efficiency insight:</span> ",
            "The optimal plan only costs <b>$",
            format(round(actual_cost / 1e6, 1)),
            "M</b> (",
            ifelse(actual_pct >= 0, "+", ""),
            actual_pct,
            "% vs reference), saving <b>$",
            format(round(savings / 1e6, 1)),
            "M</b> vs the reference budget while achieving the same cases averted."
          )),
          style = "color:#444; margin-top:6px;"
        ),
        shiny$p(
          "Click any bar above to update the map below.",
          style = "color:#888; font-style:italic;"
        )
      )
    })

    # ── Sensitivity tab: facet map at clicked budget step ──────────────────────
    # Runs a fresh LP at the clicked (or slider) budget level and renders
    # the intervention facet map. Falls back to a "budget too low" message
    # if no feasible allocation exists.
    output$sensitivity_facet <- shiny$renderPlot({
      pct <- if (!is.null(clicked_step()) && clicked_step() != 0) {
        clicked_step()
      } else {
        budget_adj_d()
      }
      bud <- budget_metrics()$curr * (1 + pct / 100)

      opt_res <- tryCatch(
        optimal_allocation(
          df = res_mean_dedup(),
          budget_env = bud,
          region_name = "admin_2",
          policy_name = "scenario_name",
          cost_name = "avg_cost",
          health_name = "averted",
          optim_dir = "max"
        ),
        error = function(e) NULL
      )

      if (is.null(opt_res) || nrow(opt_res) == 0L) {
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              size = 6,
              color = "#888",
              label = "Budget too low \u2014 all districts remain on reference plan"
            ) +
            theme_void()
        )
      }

      opt_choices <- get_best_allocation(opt_res)
      plot_dt_long <- prepare_facet_data(
        opt_choices,
        ee_data(),
        variables$intervention_cols
      )
      make_facet_map(plot_dt_long, country_map_sf(), country_outline())
    })

    # ── Planner tab: reference budget hint ─────────────────────────────────────

    output$planner_ref_budget_hint <- shiny$renderUI({
      shiny$req(budget_metrics())
      shiny$p(HTML(paste0(
        "<b>Reference budget (BAU):</b> $",
        format(round(budget_metrics()$curr), big.mark = ",")
      )))
    })

    # ── Planner tab: value boxes ───────────────────────────────────────────────
    # Shows entered budget vs BAU, cases averted, actual plan cost, and
    # unspent funds.
    output$planner_value_boxes <- shiny$renderUI({
      shiny$req(planner_results(), budget_metrics())
      data <- planner_results()
      entered <- attr(data, "entered_budget")
      bau <- budget_metrics()$curr
      diff <- entered - bau
      sign <- ifelse(diff >= 0, "+", "-")
      cases_averted <- sum(data$health_val, na.rm = TRUE)
      actual_cost <- sum(data$cost_val, na.rm = TRUE)
      unspent <- entered - actual_cost
      warn <- attr(data, "warning_msg")

      fmt_usd <- function(x) {
        paste0("$", prettyNum(round(x), big.mark = ","))
      }

      shiny$tagList(
        shiny$fluidRow(
          shiny$column(
            6,
            bslib$value_box(
              title = paste0(
                "Budget Entered | ",
                sign,
                prettyNum(round(abs(diff)), big.mark = ","),
                " vs BAU (",
                fmt_usd(bau),
                ")"
              ),
              value = fmt_usd(entered),
              theme = "warning"
            )
          ),
          shiny$column(
            6,
            bslib$value_box(
              title = if (!is.null(warn)) {
                warn
              } else {
                paste0("Cases Averted vs ", input$reference_plan, " reference")
              },
              value = format(
                round(cases_averted),
                big.mark = ","
              ),
              theme = if (cases_averted > 0) "success" else "danger"
            )
          )
        ),
        shiny$fluidRow(
          shiny$column(
            6,
            bslib$value_box(
              title = if (unspent > 100) {
                paste0(
                  "Actual plan cost | ",
                  fmt_usd(unspent),
                  " unspent"
                )
              } else {
                "Actual plan cost | Budget fully utilised"
              },
              value = fmt_usd(actual_cost),
              theme = "primary"
            )
          ),
          shiny$column(
            6,
            bslib$value_box(
              title = if (unspent > 100) {
                "Unspent \u2014 no better scenario fits remaining funds"
              } else {
                "Budget fully utilised by optimizer"
              },
              value = fmt_usd(max(0, unspent)),
              theme = if (unspent > 100) "secondary" else "success"
            )
          )
        )
      )
    })

    # ── Planner tab: allocation map ────────────────────────────────────────────
    output$map_planner <- renderLeaflet({
      shiny$req(planner_results())
      data <- planner_results()
      map_obj <- build_map_obj(data, country_map_sf())

      map_obj$disp_label <- lapply(
        paste0(
          "<b>",
          map_obj$JOIN_TARGET,
          "</b><br>",
          "<b>Status:</b> ",
          map_obj$final_plan,
          "<br>",
          "<b>Interventions deployed:</b> ",
          map_obj$final_tools
        ),
        HTML
      )

      all_labels <- c("Reference", "Added", "Reduced", "Substituted")
      pal <- colorFactor(
        palette = c("#756bb1", "#2ca25f", "#e67e22", "#2980b9"),
        levels = all_labels
      )

      leaflet(map_obj) |>
        addProviderTiles(providers$CartoDB.PositronNoLabels) |>
        addPolygons(
          fillColor = ~ pal(final_plan),
          weight = 1,
          color = "white",
          fillOpacity = 0.7,
          label = ~disp_label,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            bringToFront = TRUE
          )
        ) |>
        addLegend(
          pal = pal,
          values = all_labels,
          title = HTML(
            "Planner Strategy<br>",
            "<small style='font-weight:normal;font-style:italic;'>",
            "Hover district for interventions</small>"
          )
        )
    })
  })
}


# Helpers (module-private) -----------------------------------------------------

#' Shared map legend HTML block
map_legend_html <- function() {
  shiny$div(
    shiny$HTML(
      paste0(
        "<b style='color:#756bb1;'>&#9632;</b> <b>Reference:</b> ",
        "Optimizer selected the reference plan.<br>",
        "<b style='color:#2ca25f;'>&#9632;</b> <b>Added:</b> ",
        "Optimal plan has <i>more</i> interventions.<br>",
        "<b style='color:#e67e22;'>&#9632;</b> <b>Reduced:</b> ",
        "Optimal plan has <i>fewer</i> interventions.<br>",
        "<b style='color:#2980b9;'>&#9632;</b> <b>Substituted:</b> ",
        "Optimal plan has a <i>different mix</i>."
      )
    ),
    style = "color: #555; font-style: italic; margin: 6px 10px 4px 10px;"
  )
}
