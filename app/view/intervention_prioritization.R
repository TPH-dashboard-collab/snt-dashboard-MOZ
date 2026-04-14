# fmt: skip
box::use(
  config,
  bslib,
  data.table[
    `:=`,
    `.N`,
    `.SD`,
    copy,
    data.table,
    frank,
    merge.data.table,
    rbindlist,
    setnames,
    setorder,
    setorderv,
    uniqueN
  ],
  DT[datatable, DTOutput, renderDT],
  ggplot2[
    aes,
    coord_flip,
    element_blank,
    element_line,
    element_text,
    expand_limits,
    geom_bar,
    geom_text,
    ggplot,
    labs,
    scale_fill_brewer,
    scale_fill_manual,
    theme,
    theme_minimal
  ],
  htmltools[HTML],
  later,
  leaflet[
    addLegend,
    addPolygons,
    addProviderTiles,
    colorFactor,
    highlightOptions,
    labelOptions,
    leaflet,
    leafletOutput,
    providers,
    renderLeaflet
  ],
  logger[log_debug],
  sf[st_as_sf],
  stats[reorder, setNames],
  shiny,
  utils[head]
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/interventions/intervention_combo_filter[generate_counterfactual_combos],
  app/logic/interventions/intervention_impact[
    per_interv_impact,
    rank_groups,
    rank_numeric
  ],
  app/logic/visualization/color_palettes[create_blue_palette],
)

.datatable.aware <- TRUE # nolint

# Colour palette for rank groups (using dynamic palette function)
RANK_GROUP_NAMES <- c("1-2", "3-4", "5-6", "7-8", "9-10")
RANK_COLORS <- setNames(
  rev(create_blue_palette(5)), # Reversed: darkest = highest priority

  RANK_GROUP_NAMES
)

#' Create placeholder UI for unavailable analysis
#'
#' @param height CSS height for the placeholder container
#' @param message Custom message to display (optional)
#' @return A shiny tagList with styled placeholder
#' @keywords internal
create_analysis_placeholder <- function(
  height = "400px",
  message = NULL
) {
  default_message <- paste(
    "Analysis not available for the current settings.",
    "Please select different filtering options"
  )

  shiny$div(
    style = paste0(
      "height: ",
      height,
      "; ",
      "display: flex; ",
      "flex-direction: column; ",
      "align-items: center; ",
      "justify-content: center; ",
      "background-color: #f8f9fa; ",
      "border: 1px dashed #dee2e6; ",
      "border-radius: 8px; ",
      "color: #6c757d; ",
      "text-align: center; ",
      "padding: 20px;"
    ),
    shiny$div(
      style = "font-size: 48px; margin-bottom: 16px; opacity: 0.5;",
      shiny$icon("chart-bar")
    ),
    shiny$div(
      style = "font-size: 16px; font-weight: 500; margin-bottom: 8px;",
      "Analysis Unavailable"
    ),
    shiny$div(
      style = "font-size: 14px; max-width: 400px; line-height: 1.5;",
      if (!is.null(message)) message else default_message
    )
  )
}

#' Creates the UI component for intervention prioritization
#'
#' @param id The module ID used for namespacing
#' @return A Shiny UI component containing the intervention prioritization tabs
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    # Inline filters row
    bslib$card(
      bslib$card_body(
        bslib$layout_column_wrap(
          width = 1 / 3,
          # 24-03-2026 NOTE: in the future we might want to have
          # more flexible metrics (eg prevalence rate, severe cases)
          shiny$selectInput(
            inputId = ns("metric_filter"),
            label = "Ranking Metric:",
            choices = c(
              "Cases Averted" = "mean_impact",
              "Cases per 1,000 pop." = "impact_per_1000"
            ),
            selected = "mean_impact"
          ),
          # Use uiOutput instead of selectInput to avoid race conditions
          # with updateSelectInput. The selectInput is rendered dynamically
          # in the server with the correct choices from the start.
          shiny$uiOutput(ns("intervention_filter_ui")),
          #checkmark:
          # top n or all (then the quantiles are displayed)
          #if top n then slider activated:
          shiny$sliderInput(
            inputId = ns("top_n"),
            label = "Top N Districts:",
            min = 5,
            max = 20,
            value = 10,
            step = 1
          )
        )
      )
    ),

    # Main content tabs
    bslib$navset_card_pill(
      id = ns("main_tabs"),
      title = "District Prioritization by Intervention",
      # Explore tab
      bslib$nav_panel(
        "Explore",
        shiny$uiOutput(ns("explore_subtitle")),
        bslib$layout_column_wrap(
          width = 1 / 2,
          bslib$card(
            full_screen = TRUE,
            bslib$card_header(shiny$uiOutput(ns("map_title"))),
            shiny$uiOutput(ns("map_explore_container"))
          ),
          bslib$card(
            full_screen = TRUE,
            bslib$card_header(shiny$uiOutput(ns("chart_title"))),
            shiny$uiOutput(ns("plot_ranking_container"))
          )
        )
      ),
      # Overview tab
      bslib$nav_panel(
        "Overview",
        bslib$card(
          full_screen = TRUE,
          bslib$card_header(shiny$uiOutput(ns("home_chart_title"))),
          shiny$uiOutput(ns("plot_home_top_container"))
        )
      )
    )
  )
}

#' Creates the server logic for intervention prioritization
#'
#' @param id The module ID
#' @param variables Application variables containing data connection and state
#' @param country_map_dt Geographic data for map rendering
#' @param use_custom_data Logical, use custom data from session state
#' @param trigger Reactive trigger for updates
#' @param year_start Reactive for start year filter
#' @param year_end Reactive for end year filter
#' @param ... Additional arguments including log_ns
#' @export
server <- function(
  id,
  variables,
  country_map_dt,
  use_custom_data = FALSE,
  trigger,
  year_start = NULL,
  year_end = NULL,
  toggle_enabled = NULL,
  ...
) {
  shiny$moduleServer(id, function(input, output, session) {
    args <- list(...)
    log_ns <- log_ns_builder(args, "intervention_prioritization")

    # Validate toggle_enabled parameter
    if (is.null(toggle_enabled)) {
      stop(
        "toggle_enabled parameter is required for intervention_prioritization module"
      )
    }

    # ========================================================================
    # HELPERS
    # ========================================================================

    # Get age_group from variable state
    age_filter <- shiny$reactive({
      shiny$req(variables$session_state$age_group)
      variables$session_state$age_group
    })

    # Get year_start from parameter or variable state
    year_start_filter <- shiny$reactive({
      if (!is.null(year_start)) {
        shiny$req(year_start())
        year_start()
      } else {
        shiny$req(variables$session_state$year_start)
        variables$session_state$year_start
      }
    })

    # Get year_end from parameter or variable state
    year_end_filter <- shiny$reactive({
      if (!is.null(year_end)) {
        shiny$req(year_end())
        year_end()
      } else {
        shiny$req(variables$session_state$year_end)
        variables$session_state$year_end
      }
    })

    # Active metric column name
    metric_col <- shiny$reactive({
      input$metric_filter
    })

    # Active rank column names (consolidated)
    rank_columns <- shiny$reactive({
      suffix <- if (input$metric_filter == "impact_per_1000") {
        "_per_1000"
      } else {
        "_cases"
      }
      list(
        group = paste0("rank_group", suffix),
        num = paste0("rank_num", suffix),
        abs = paste0("rank", suffix)
      )
    })

    metric_label <- shiny$reactive({
      if (input$metric_filter == "impact_per_1000") {
        "Cases Averted per 1,000 pop."
      } else {
        "Cases Averted"
      }
    })

    age_label <- shiny$reactive({
      ag <- age_filter()
      if (ag == "0-100") {
        "All ages"
      } else {
        "Children (0-5)"
      }
    })

    # Display name for selected scenario
    scenario_display_name <- shiny$reactive({
      shiny$req(variables$session_state$scenario_selected)
      variables$scenario_mapping[variables$session_state$scenario_selected]
    })

    # Unified trigger that responds to all update sources and filter changes
    # This ensures both data fetching and impact calculations happen when:
    # - Panel 2: trigger() fires (Update button or init) OR any filter changes
    # - Panel 3: custom_data_trigger() fires (Apply Selections) OR any filter changes
    # - Filters: scenario, age, aggregation level, region, strata, year range
    unified_update_trigger <- shiny$reactive({
      if (use_custom_data) {
        # Panel 3 mode: respond to custom_data_trigger + scenario changes
        shiny$req(
          variables$session_state$custom_data,
          ncol(variables$session_state$custom_data) > 1
        )
        variables$custom_data_trigger()
      } else {
        # Panel 2 mode: respond to trigger + scenario changes
        trigger()
      }
      # Include all filter dependencies for both modes
      # These will cause recalculation when filters change
      list(
        scenario = variables$session_state$scenario_selected,
        age = variables$session_state$age_group,
        agg_level = variables$session_state$agg_level,
        region = variables$session_state$region_selected,
        strata = variables$session_state$strata_selected,
        year_start = if (!is.null(year_start)) {
          year_start()
        } else {
          variables$session_state$year_start
        },
        year_end = if (!is.null(year_end)) {
          year_end()
        } else {
          variables$session_state$year_end
        }
      )
    })

    # Conditional trigger that only fires when toggle is ON
    # This prevents calculations from running when feature is not visible
    conditional_trigger <- shiny$reactive({
      # Block execution if toggle is FALSE/NULL
      shiny$req(toggle_enabled())

      # If toggle is ON, pass through the unified trigger
      unified_update_trigger()
    })

    # ========================================================================
    # DATA FETCHING
    # ========================================================================

    # Fetch prioritization data from database or custom data
    prioritization_data <- shiny$reactive({
      # Depend on unified_update_trigger to refetch when any filter changes
      unified_update_trigger()

      shiny$req(
        variables$session_state$age_group,
        variables$session_state$scenario_selected
      )

      log_debug(
        "Fetching prioritization data...",
        namespace = log_ns_fn(log_ns)
      )

      if (use_custom_data) {
        # Panel 3: Start with custom_data but need to fetch counterfactuals
        custom_dt <- variables$session_state$custom_data

        # Step 1: Get "custom" scenario data only for counterfactual generation
        custom_scenario_dt <- custom_dt[scenario_name == "custom"]

        log_debug(
          paste("Custom scenario data:", nrow(custom_scenario_dt), "rows"),
          namespace = log_ns_fn(log_ns)
        )

        # Get unique districts from custom scenario
        districts <- unique(custom_scenario_dt$admin_2)

        log_debug(
          paste("Custom scenario has", length(districts), "districts"),
          namespace = log_ns_fn(log_ns)
        )

        if (length(districts) == 0) {
          log_debug(
            "WARNING: No districts found in custom scenario data.",
            namespace = log_ns_fn(log_ns)
          )
          return(data.table())
        }

        # Step 2: Extract ALL unique intervention combinations from custom scenario
        interv_cols <- variables$intervention_cols
        unique_patterns <- unique(custom_scenario_dt[, ..interv_cols])

        log_debug(
          paste(
            "Found",
            nrow(unique_patterns),
            "unique intervention patterns in custom scenario"
          ),
          namespace = log_ns_fn(log_ns)
        )

        # Step 3: Generate counterfactual combinations for ALL unique patterns
        all_combos <- list()
        for (i in seq_len(nrow(unique_patterns))) {
          target_combo <- as.logical(unique_patterns[i])
          names(target_combo) <- interv_cols
          pattern_combos <- generate_counterfactual_combos(
            target = target_combo,
            base_interventions = config$get("base_interventions")
          )
          all_combos <- c(all_combos, pattern_combos)
        }
        # Remove duplicates (same combo might be generated from different patterns)
        all_combos <- unique(all_combos)

        log_debug(
          paste(
            "Generated",
            length(all_combos),
            "total intervention combinations for counterfactuals"
          ),
          namespace = log_ns_fn(log_ns)
        )

        # Step 4: Fetch counterfactual scenarios from full factorial plan
        counterfactual_filters <- list(
          age_group = variables$session_state$age_group,
          admin_2 = districts,
          plan = config$get("plans")$full_factorial
        )
        if (
          variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
            length(variables$session_state$region_selected) > 0
        ) {
          counterfactual_filters <- c(
            counterfactual_filters,
            list(admin_1 = variables$session_state$region_selected)
          )
        }
        if (
          variables$session_state$agg_level == "Risk Strata" &&
            length(variables$session_state$strata_selected) > 0
        ) {
          counterfactual_filters <- c(
            counterfactual_filters,
            list(risk_stratum = variables$session_state$strata_selected)
          )
        }

        counterfactual_dt <- variables$data_con$get_counterfactual_data(
          filters = counterfactual_filters,
          intervention_combos = all_combos,
          conversion_rules = variables$conversion_rules,
          log_ns = log_ns
        )

        log_debug(
          paste(
            "Counterfactual data from full factorial plan:",
            nrow(counterfactual_dt),
            "rows"
          ),
          namespace = log_ns_fn(log_ns)
        )

        # Step 5: Combine custom scenario (from custom_data) with counterfactuals
        # Note: We use custom_scenario_dt which has scenario_name = "custom"
        dt <- rbindlist(
          list(custom_scenario_dt, counterfactual_dt),
          fill = TRUE
        )

        log_debug(
          paste(
            "Combined data - plans:",
            paste(unique(dt$plan), collapse = ", ")
          ),
          namespace = log_ns_fn(log_ns)
        )
      } else {
        # Step 1: Get districts from selected scenario only
        # NOTE: NO year filter - impact calculation needs all years for cumulative metrics
        scenario_filters <- list(
          age_group = variables$session_state$age_group,
          scenario_name = variables$session_state$scenario_selected
        )
        if (
          variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
            length(variables$session_state$region_selected) > 0
        ) {
          scenario_filters <- c(
            scenario_filters,
            list(admin_1 = variables$session_state$region_selected)
          )
        }
        if (
          variables$session_state$agg_level == "Risk Strata" &&
            length(variables$session_state$strata_selected) > 0
        ) {
          scenario_filters <- c(
            scenario_filters,
            list(risk_stratum = variables$session_state$strata_selected)
          )
        }

        log_debug(
          paste(
            "Scenario filters:",
            paste(
              names(scenario_filters),
              "=",
              sapply(scenario_filters, function(x) paste(x, collapse = ",")),
              collapse = "; "
            )
          ),
          namespace = log_ns_fn(log_ns)
        )

        scenario_dt <- variables$data_con$get_filtered_data(
          filters = scenario_filters,
          conversion_rules = variables$conversion_rules,
          log_ns = log_ns
        )

        log_debug(
          paste("Scenario data fetched:", nrow(scenario_dt), "rows"),
          namespace = log_ns_fn(log_ns)
        )

        # Get unique districts from selected scenario
        districts <- unique(scenario_dt$admin_2)

        log_debug(
          paste("Selected scenario has", length(districts), "districts"),
          namespace = log_ns_fn(log_ns)
        )

        # Check if we have any districts
        if (length(districts) == 0) {
          log_debug(
            "WARNING: No districts found in scenario data. Check if filters are too restrictive.",
            namespace = log_ns_fn(log_ns)
          )
          # Return empty data.table with proper structure
          return(data.table())
        }

        # Step 2: Extract ALL unique intervention combinations from selected scenario
        # Different districts may have different intervention patterns
        interv_cols <- variables$intervention_cols
        unique_patterns <- unique(scenario_dt[, ..interv_cols])

        log_debug(
          paste("Found", nrow(unique_patterns), "unique intervention patterns"),
          namespace = log_ns_fn(log_ns)
        )

        # Extract plan value
        selected_plan <- unique(scenario_dt$plan)
        if (length(selected_plan) > 1) {
          log_debug(
            paste(
              "Warning: Multiple plans found in scenario:",
              paste(selected_plan, collapse = ", ")
            ),
            namespace = log_ns_fn(log_ns)
          )
          selected_plan <- selected_plan[1]
        }
        log_debug(
          paste("Selected plan:", selected_plan),
          namespace = log_ns_fn(log_ns)
        )

        # Step 3: Generate counterfactual combinations for ALL unique patterns
        all_combos <- list()
        for (i in seq_len(nrow(unique_patterns))) {
          target_combo <- as.logical(unique_patterns[i])
          names(target_combo) <- interv_cols
          pattern_combos <- generate_counterfactual_combos(
            target = target_combo,
            base_interventions = config$get("base_interventions")
          )
          all_combos <- c(all_combos, pattern_combos)
        }
        # Remove duplicates (same combo might be generated from different patterns)
        all_combos <- unique(all_combos)

        log_debug(
          paste(
            "Generated",
            length(all_combos),
            "total intervention combinations"
          ),
          namespace = log_ns_fn(log_ns)
        )

        # Step 4: Fetch counterfactual scenarios from full factorial plan
        # Counterfactuals are stored with all intervention combinations
        # NOTE: NO year filter - impact calculation needs all years for cumulative metrics
        counterfactual_filters <- list(
          age_group = variables$session_state$age_group,
          admin_2 = districts,
          plan = config$get("plans")$full_factorial
        )
        if (
          variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
            length(variables$session_state$region_selected) > 0
        ) {
          counterfactual_filters <- c(
            counterfactual_filters,
            list(admin_1 = variables$session_state$region_selected)
          )
        }
        if (
          variables$session_state$agg_level == "Risk Strata" &&
            length(variables$session_state$strata_selected) > 0
        ) {
          counterfactual_filters <- c(
            counterfactual_filters,
            list(risk_stratum = variables$session_state$strata_selected)
          )
        }

        counterfactual_dt <- variables$data_con$get_counterfactual_data(
          filters = counterfactual_filters,
          intervention_combos = all_combos,
          conversion_rules = variables$conversion_rules,
          log_ns = log_ns
        )

        log_debug(
          paste(
            "Counterfactual data from full factorial plan:",
            nrow(counterfactual_dt),
            "rows"
          ),
          namespace = log_ns_fn(log_ns)
        )
        log_debug(
          paste(
            "Unique scenario_names in counterfactual data:",
            paste(unique(counterfactual_dt$scenario_name), collapse = ", ")
          ),
          namespace = log_ns_fn(log_ns)
        )

        # Step 5: Combine baseline (selected plan) with counterfactuals (full factorial)
        dt <- rbindlist(list(scenario_dt, counterfactual_dt), fill = TRUE)

        # Debug: Log combined data
        log_debug(
          paste(
            "Combined data - plans:",
            paste(unique(dt$plan), collapse = ", ")
          ),
          namespace = log_ns_fn(log_ns)
        )
      }

      log_debug(
        paste("Prioritization data fetched:", nrow(dt), "rows"),
        namespace = log_ns_fn(log_ns)
      )
      dt
    })

    # Generate strata_all from unique combinations in the data
    strata_all <- shiny$reactive({
      shiny$req(
        prioritization_data(),
        variables$session_state$scenario_selected
      )
      dt <- prioritization_data()

      # Get unique combinations of admin_2, age_group, scenario_name
      # Filter to only include the selected scenario (not counterfactuals)
      # For panel 3 (use_custom_data=TRUE), the scenario is "custom" not scenario_selected
      target_scenario <- if (use_custom_data) {
        "custom"
      } else {
        variables$session_state$scenario_selected
      }
      strata <- unique(dt[
        scenario_name == target_scenario,
        .(admin_2, age_group, scenario_name)
      ])

      log_debug(
        paste(
          "Generated strata_all:",
          nrow(strata),
          "combinations",
          "(target scenario:",
          target_scenario,
          ")"
        ),
        namespace = log_ns_fn(log_ns)
      )
      strata
    })

    # Extract plan from prioritization data for baseline filtering
    # For panel 3 (custom data), return NULL since:
    # - The "custom" scenario is already filtered by scenario_name
    # - Baseline data doesn't need plan filtering
    # - Counterfactuals use plans$full_factorial from config in per_interv_impact
    selected_plan <- shiny$reactive({
      # Panel 3: Don't filter by plan for baseline - scenario_name is sufficient
      if (use_custom_data) {
        log_debug(
          "Panel 3 mode: using NULL plan (filtering by scenario_name 'custom' instead)",
          namespace = log_ns_fn(log_ns)
        )
        return(NULL)
      }

      # Panel 2: Extract plan from selected scenario data
      shiny$req(prioritization_data())
      dt <- prioritization_data()
      # Filter to selected scenario to get the correct plan
      scenario_dt <- dt[
        scenario_name == variables$session_state$scenario_selected
      ]
      plans <- unique(scenario_dt$plan)
      if (length(plans) > 1) {
        log_debug(
          paste(
            "Multiple plans in selected scenario:",
            paste(plans, collapse = ", ")
          ),
          namespace = log_ns_fn(log_ns)
        )
      }
      if (length(plans) == 0) {
        log_debug(
          "WARNING: No plans found in selected scenario data",
          namespace = log_ns_fn(log_ns)
        )
        return(NULL)
      }
      plans[1]
    })

    # Extract population data (nHost) from prioritization data
    population_data <- shiny$reactive({
      shiny$req(prioritization_data())
      dt <- prioritization_data()

      # Get unique population by admin_2 and age_group
      # Use mean nHost across scenarios/years as the population estimate
      pop_dt <- dt[,
        .(nHost = mean(nHost, na.rm = TRUE)),
        by = .(admin_2, age_group)
      ]

      log_debug(
        paste("Population data extracted:", nrow(pop_dt), "rows"),
        namespace = log_ns_fn(log_ns)
      )
      pop_dt
    })

    # ========================================================================
    # REACTIVE IMPACT CALCULATION
    # ========================================================================

    impacts_reactive <- shiny$reactiveVal(NULL)

    shiny$observeEvent(
      conditional_trigger(),
      {
        log_debug(
          "conditional_trigger fired - starting impact calculation",
          namespace = log_ns_fn(log_ns)
        )

        shiny$req(
          strata_all(),
          prioritization_data(),
          population_data(),
          variables$session_state$scenario_selected
        )
        # Note: selected_plan() can be NULL for panel 3, so don't require it

        log_debug(
          "All prerequisites met for impact calculation",
          namespace = log_ns_fn(log_ns)
        )

        # For panel 3 (custom data), use "custom" as target scenario
        # For panel 2, use the selected scenario name
        target_scenario <- if (use_custom_data) {
          "custom"
        } else {
          variables$session_state$scenario_selected
        }
        current_plan <- selected_plan()
        strata_plan <- strata_all()[scenario_name == target_scenario]

        log_debug(
          paste(
            "Using plan for baseline filtering:",
            if (is.null(current_plan)) "NULL (panel 3 mode)" else current_plan,
            "| target scenario:",
            target_scenario
          ),
          namespace = log_ns_fn(log_ns)
        )

        # Display name for progress message
        display_name <- if (use_custom_data) {
          "Custom Scenario"
        } else {
          variables$scenario_mapping[variables$session_state$scenario_selected]
        }

        shiny$withProgress(
          message = paste(
            "Calculating impacts for",
            display_name,
            "..."
          ),
          value = 0,
          {
            n <- nrow(strata_plan)

            log_debug(
              paste("Calculating impacts for", n, "strata"),
              namespace = log_ns_fn(log_ns)
            )

            # Filter data by year range for cumulative calculations
            # per_interv_impact will use max year from the filtered dataset
            data_for_impact <- prioritization_data()
            if (!is.null(year_end)) {
              max_year <- year_end_filter()
              data_for_impact <- data_for_impact[year <= max_year]
              log_debug(
                paste(
                  "Filtered data to years <=",
                  max_year,
                  "for cumulative impact"
                ),
                namespace = log_ns_fn(log_ns)
              )
            }

            # lapply + rbindlist - faster than for loop
            results_list <- lapply(seq_len(n), function(i) {
              shiny$incProgress(1 / n, detail = paste(i, "/", n))
              x <- strata_plan[i]
              result <- tryCatch(
                per_interv_impact(
                  data_for_impact,
                  indicator = "cum_nUncomp",
                  strata = c(
                    admin_2 = x$admin_2,
                    age_group = x$age_group,
                    scenario_name = x$scenario_name
                  ),
                  interv = variables$intervention_cols,
                  plan = current_plan,
                  base_interventions = config$get("base_interventions"),
                  log_ns = if (i == 1) log_ns_fn(log_ns) else NULL
                ),
                error = function(e) {
                  log_debug(
                    paste(
                      "Error calculating impact for",
                      x$admin_2,
                      x$age_group,
                      x$scenario_name,
                      ":",
                      e$message
                    ),
                    namespace = log_ns_fn(log_ns)
                  )
                  NULL
                }
              )
              # Log first result for debugging
              if (i == 1) {
                log_debug(
                  paste(
                    "First strata result:",
                    if (is.null(result)) {
                      "NULL"
                    } else {
                      paste(
                        nrow(result),
                        "rows,",
                        ncol(result),
                        "cols"
                      )
                    }
                  ),
                  namespace = log_ns_fn(log_ns)
                )
              }
              result
            })
            impacts_dt <- rbindlist(results_list, fill = TRUE)

            log_debug(
              paste(
                "Impact calculation result:",
                nrow(impacts_dt),
                "rows,",
                ncol(impacts_dt),
                "columns"
              ),
              namespace = log_ns_fn(log_ns)
            )
            log_debug(
              paste(
                "Impact columns:",
                paste(names(impacts_dt), collapse = ", ")
              ),
              namespace = log_ns_fn(log_ns)
            )
            if (nrow(impacts_dt) > 0) {
              log_debug(
                paste(
                  "Unique scenario_names in impacts:",
                  paste(unique(impacts_dt$scenario_name), collapse = ", ")
                ),
                namespace = log_ns_fn(log_ns)
              )
            }

            # Check if impacts_dt has data
            if (nrow(impacts_dt) == 0 || ncol(impacts_dt) == 0) {
              log_debug(
                "No valid impacts calculated - all strata returned NULL",
                namespace = log_ns_fn(log_ns)
              )
              impacts_reactive(NULL)
              return()
            }

            # Join plan back and rename value column
            impacts_dt <- merge.data.table(
              impacts_dt,
              unique(strata_plan[, .(admin_2, age_group, scenario_name)]),
              by = c("admin_2", "age_group", "scenario_name")
            )
            setnames(impacts_dt, "value", "mean_impact")

            # strip deployed_int_
            impacts_dt[,
              intervention := gsub(
                "^(deployed_int_)",
                "",
                intervention
              )
            ]

            # Population weighting - data.table join
            impacts_dt <- merge.data.table(
              impacts_dt,
              population_data(),
              by = c("admin_2", "age_group"),
              all.x = TRUE
            )
            impacts_dt[, impact_per_1000 := (mean_impact / nHost) * 1000]

            # Ranks - data.table grouped operation
            impacts_dt[,
              rank_cases := frank(-mean_impact, ties.method = "min"),
              by = .(scenario_name, intervention, age_group)
            ]
            impacts_dt[,
              rank_per_1000 := frank(-impact_per_1000, ties.method = "min"),
              by = .(scenario_name, intervention, age_group)
            ]
            impacts_dt[,
              n_districts := .N,
              by = .(scenario_name, intervention, age_group)
            ]

            impacts_dt[, rank_group_cases := rank_groups(rank_cases)]
            impacts_dt[, rank_group_per_1000 := rank_groups(rank_per_1000)]
            impacts_dt[, rank_num_cases := rank_numeric(rank_cases)]
            impacts_dt[, rank_num_per_1000 := rank_numeric(rank_per_1000)]

            log_debug(
              paste("Impact calculation complete:", nrow(impacts_dt), "rows"),
              namespace = log_ns_fn(log_ns)
            )

            impacts_reactive(impacts_dt)
          }
        )
      },
      ignoreNULL = FALSE
    )

    # ========================================================================
    # BASE REACTIVE - filtered ranks for selected scenario + age
    # ========================================================================

    ranks_filtered <- shiny$reactive({
      shiny$req(
        impacts_reactive(),
        age_filter(),
        variables$session_state$scenario_selected
      )
      dt <- impacts_reactive()
      # For panel 3, impacts have scenario_name = "custom", not scenario_selected
      scenario_sel <- if (use_custom_data) {
        "custom"
      } else {
        variables$session_state$scenario_selected
      }
      age_sel <- age_filter()

      log_debug(
        paste(
          "ranks_filtered - impacts rows:",
          nrow(dt),
          "| target scenario:",
          scenario_sel,
          "| age_filter:",
          age_sel
        ),
        namespace = log_ns_fn(log_ns)
      )
      log_debug(
        paste(
          "ranks_filtered - unique scenarios in impacts:",
          paste(unique(dt$scenario_name), collapse = ", ")
        ),
        namespace = log_ns_fn(log_ns)
      )
      log_debug(
        paste(
          "ranks_filtered - unique age_groups in impacts:",
          paste(unique(dt$age_group), collapse = ", ")
        ),
        namespace = log_ns_fn(log_ns)
      )

      filtered <- dt[scenario_name == scenario_sel & age_group == age_sel]
      log_debug(
        paste("ranks_filtered - filtered rows:", nrow(filtered)),
        namespace = log_ns_fn(log_ns)
      )
      if (nrow(filtered) > 0) {
        log_debug(
          paste(
            "ranks_filtered - interventions:",
            paste(unique(filtered$intervention), collapse = ", ")
          ),
          namespace = log_ns_fn(log_ns)
        )
      }
      filtered
    })

    # For selected intervention only
    ranks_intervention <- shiny$reactive({
      dt <- ranks_filtered()
      interv_sel <- input$intervention_filter

      # If intervention_filter is not set yet, return empty data.table
      if (is.null(interv_sel) || length(interv_sel) == 0 || interv_sel == "") {
        log_debug(
          "ranks_intervention - intervention_filter not set yet",
          namespace = log_ns_fn(log_ns)
        )
        return(dt[0])
      }

      log_debug(
        paste(
          "ranks_intervention - intervention_filter:",
          interv_sel,
          "| ranks_filtered rows:",
          nrow(dt)
        ),
        namespace = log_ns_fn(log_ns)
      )
      filtered <- dt[intervention == interv_sel]
      log_debug(
        paste("ranks_intervention - filtered rows:", nrow(filtered)),
        namespace = log_ns_fn(log_ns)
      )
      filtered
    })

    # ========================================================================
    # INTERVENTION FILTER - Dynamically rendered selectInput
    # ========================================================================

    # Use renderUI instead of updateSelectInput to avoid race conditions.
    # The selectInput is rendered with correct choices from the start,
    # eliminating timing issues between server updates and client rendering.
    output$intervention_filter_ui <- shiny$renderUI({
      log_debug(
        "intervention_filter_ui renderUI STARTED",
        namespace = log_ns_fn(log_ns)
      )

      # Get impacts data
      impacts_data <- impacts_reactive()

      # Get age and scenario directly from session state
      # For panel 3, impacts have scenario_name = "custom"
      age_sel <- variables$session_state$age_group
      scenario_sel <- if (use_custom_data) {
        "custom"
      } else {
        variables$session_state$scenario_selected
      }

      log_debug(
        paste(
          "intervention_filter_ui - impacts:",
          if (is.null(impacts_data)) {
            "NULL"
          } else {
            paste(nrow(impacts_data), "rows")
          },
          "| age:",
          if (is.null(age_sel)) "NULL" else age_sel,
          "| scenario:",
          if (is.null(scenario_sel)) "NULL" else scenario_sel
        ),
        namespace = log_ns_fn(log_ns)
      )

      # If no data yet, show loading state
      if (
        is.null(impacts_data) ||
          nrow(impacts_data) == 0 ||
          is.null(age_sel) ||
          is.null(scenario_sel)
      ) {
        log_debug(
          "intervention_filter_ui - showing loading state",
          namespace = log_ns_fn(log_ns)
        )
        return(
          shiny$selectInput(
            inputId = session$ns("intervention_filter"),
            label = "Intervention:",
            choices = c("Loading..." = ""),
            selected = ""
          )
        )
      }

      # Filter the data
      dt <- impacts_data[scenario_name == scenario_sel & age_group == age_sel]
      log_debug(
        paste("intervention_filter_ui - filtered data:", nrow(dt), "rows"),
        namespace = log_ns_fn(log_ns)
      )

      # Get unique intervention choices
      choices <- sort(unique(dt$intervention))
      log_debug(
        paste(
          "intervention_filter_ui - choices:",
          length(choices),
          "-",
          paste(choices, collapse = ", ")
        ),
        namespace = log_ns_fn(log_ns)
      )

      # If no choices, show empty state
      if (length(choices) == 0) {
        log_debug(
          "intervention_filter_ui - no choices available",
          namespace = log_ns_fn(log_ns)
        )
        return(
          shiny$selectInput(
            inputId = session$ns("intervention_filter"),
            label = "Intervention:",
            choices = c("No interventions" = ""),
            selected = ""
          )
        )
      }

      # Render the selectInput with correct choices
      log_debug(
        paste(
          "intervention_filter_ui - RENDERING selectInput with",
          length(choices),
          "choices, selecting:",
          choices[1]
        ),
        namespace = log_ns_fn(log_ns)
      )

      shiny$selectInput(
        inputId = session$ns("intervention_filter"),
        label = "Intervention:",
        choices = choices,
        selected = choices[1]
      )
    })

    # Debug observer to track intervention_filter changes
    shiny$observe({
      val <- input$intervention_filter
      log_debug(
        paste(
          "intervention_filter INPUT CHANGED to:",
          if (is.null(val)) {
            "NULL"
          } else if (val == "") {
            "EMPTY"
          } else {
            val
          }
        ),
        namespace = log_ns_fn(log_ns)
      )
    })

    # ========================================================================
    # OVERVIEW TAB
    # ========================================================================
    output$home_chart_title <- shiny$renderUI({
      shiny$req(variables$session_state$scenario_selected)
      paste(
        "Cases Averted by Intervention -",
        scenario_display_name(),
        "|",
        age_label()
      )
    })

    # Container that shows placeholder or plot based on data availability
    output$plot_home_top_container <- shiny$renderUI({
      impacts <- impacts_reactive()

      if (is.null(impacts) || nrow(impacts) == 0) {
        create_analysis_placeholder(height = "400px")
      } else {
        shiny$plotOutput(session$ns("plot_home_top"), height = "400px")
      }
    })

    output$plot_home_top <- shiny$renderPlot({
      shiny$req(
        impacts_reactive(),
        age_filter(),
        variables$session_state$scenario_selected
      )

      # For panel 3, impacts have scenario_name = "custom", not scenario_selected
      target_scenario <- if (use_custom_data) {
        "custom"
      } else {
        variables$session_state$scenario_selected
      }

      # Filter by selected scenario + age - data.table aggregation
      data <- impacts_reactive()[
        scenario_name == target_scenario &
          age_group == age_filter(),
        .(
          total_cases = sum(mean_impact, na.rm = TRUE),
          total_pop = sum(nHost, na.rm = TRUE)
        ),
        by = intervention
      ]
      data[, weighted_per_1000 := (total_cases / total_pop) * 1000]
      data[,
        active := if (input$metric_filter == "impact_per_1000") {
          weighted_per_1000
        } else {
          total_cases
        }
      ]
      setorder(data, -active)

      y_label <- if (input$metric_filter == "impact_per_1000") {
        "Cases Averted per 1,000 people (across all districts)"
      } else {
        "Total Cases Averted (all districts)"
      }

      ggplot(
        data,
        aes(x = reorder(intervention, active), y = active, fill = intervention)
      ) +
        geom_bar(stat = "identity", alpha = 0.85, width = 0.65) +
        geom_text(
          aes(
            label = if (input$metric_filter == "impact_per_1000") {
              round(active, 2)
            } else {
              format(round(active), big.mark = ",")
            }
          ),
          hjust = -0.1,
          size = 4.5
        ) +
        scale_fill_brewer(palette = "Set2") +
        coord_flip() +
        expand_limits(y = max(data$active) * 1.18) +
        labs(
          x = "",
          y = y_label,
          subtitle = paste(
            scenario_display_name(),
            "-",
            age_label()
          )
        ) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.subtitle = element_text(color = "gray50")
        )
    })

    # ========================================================================
    # EXPLORE TAB - titles
    # ========================================================================

    output$explore_subtitle <- shiny$renderUI({
      shiny$req(variables$session_state$scenario_selected)

      # Handle case where intervention_filter is not set yet
      if (
        is.null(input$intervention_filter) ||
          length(input$intervention_filter) == 0 ||
          input$intervention_filter == ""
      ) {
        return(shiny$tags$p(
          style = "color:#666; margin-bottom: 10px;",
          "Loading..."
        ))
      }

      shiny$tags$p(
        style = "color:#666; margin-bottom: 10px;",
        shiny$tags$strong(input$intervention_filter),
        " - ",
        scenario_display_name(),
        " | ",
        "Top ",
        input$top_n,
        " districts | ",
        age_label(),
        " | ",
        metric_label()
      )
    })

    output$map_title <- shiny$renderUI({
      if (
        is.null(input$intervention_filter) ||
          length(input$intervention_filter) == 0 ||
          input$intervention_filter == ""
      ) {
        return("Priority Map")
      }
      paste("Priority Map -", input$intervention_filter)
    })

    output$chart_title <- shiny$renderUI({
      if (
        is.null(input$intervention_filter) ||
          length(input$intervention_filter) == 0 ||
          input$intervention_filter == ""
      ) {
        return("Ranking")
      }
      paste("Ranking -", input$intervention_filter)
    })

    # ========================================================================
    # EXPLORE TAB - MAP
    # ========================================================================

    # Container that shows placeholder or map based on data availability
    output$map_explore_container <- shiny$renderUI({
      impacts <- impacts_reactive()

      if (is.null(impacts) || nrow(impacts) == 0) {
        create_analysis_placeholder(height = "450px")
      } else {
        leafletOutput(session$ns("map_explore"), height = "450px")
      }
    })

    output$map_explore <- renderLeaflet({
      shiny$req(impacts_reactive())
      shiny$req(ranks_intervention())

      log_debug(
        paste(
          "map_explore rendering - intervention_filter:",
          input$intervention_filter
        ),
        namespace = log_ns_fn(log_ns)
      )

      cols <- rank_columns()
      rc <- cols$group
      rac <- cols$abs
      mc <- metric_col()

      data_interv <- ranks_intervention()
      log_debug(
        paste("map_explore - ranks_intervention rows:", nrow(data_interv)),
        namespace = log_ns_fn(log_ns)
      )

      # Validate data_interv has rows
      shiny$req(nrow(data_interv) > 0)

      # Join to shapefile - merge sf object with data.table
      map_data <- merge(
        country_map_dt,
        data_interv,
        by = "admin_2",
        all.x = TRUE
      )
      map_data <- st_as_sf(map_data)

      # Colour palette - rank group
      pal <- colorFactor(
        palette = unname(RANK_COLORS),
        levels = names(RANK_COLORS),
        na.color = "lightgray"
      )

      # Tooltip
      metric_vals <- if (mc == "impact_per_1000") {
        ifelse(
          is.na(map_data[[mc]]),
          "No data",
          as.character(round(map_data[[mc]], 2))
        )
      } else {
        ifelse(
          is.na(map_data[[mc]]),
          "No data",
          format(round(map_data[[mc]]), big.mark = ",")
        )
      }

      rank_vals <- ifelse(
        is.na(map_data[[rac]]),
        "-",
        paste0("#", map_data[[rac]])
      )
      group_vals <- ifelse(is.na(map_data[[rc]]), "-", map_data[[rc]])

      labels <- sprintf(
        "<strong>%s</strong><br/>
       <strong>Rank:</strong> %s<br/>
       <strong>Rank group:</strong> %s<br/>
       <strong>%s:</strong> %s<br/>
       <strong>Scenario:</strong> %s  -  <strong>Age:</strong> %s",
        map_data$admin_2,
        rank_vals,
        group_vals,
        metric_label(),
        metric_vals,
        scenario_display_name(),
        age_label()
      )
      labels <- lapply(labels, HTML)

      leaflet(map_data) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          fillColor = ~ pal(map_data[[rc]]),
          fillOpacity = 0.7,
          color = "black",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) |>
        addLegend(
          colors = unname(RANK_COLORS),
          labels = names(RANK_COLORS),
          opacity = 0.8,
          title = "Priority Rank",
          position = "bottomright"
        )
    })

    # ========================================================================
    # EXPLORE TAB - RANKING BAR CHART
    # ========================================================================

    # Container that shows placeholder or plot based on data availability
    output$plot_ranking_container <- shiny$renderUI({
      impacts <- impacts_reactive()

      if (is.null(impacts) || nrow(impacts) == 0) {
        create_analysis_placeholder(height = "450px")
      } else {
        shiny$plotOutput(session$ns("plot_ranking"), height = "450px")
      }
    })

    output$plot_ranking <- shiny$renderPlot({
      shiny$req(impacts_reactive())
      shiny$req(ranks_intervention())

      log_debug(
        paste(
          "plot_ranking rendering - intervention_filter:",
          input$intervention_filter
        ),
        namespace = log_ns_fn(log_ns)
      )

      cols <- rank_columns()
      rc <- cols$group
      rac <- cols$abs
      mc <- metric_col()

      data_plot <- copy(ranks_intervention())
      log_debug(
        paste("plot_ranking - data_plot rows:", nrow(data_plot)),
        namespace = log_ns_fn(log_ns)
      )

      # Validate data_plot has rows
      shiny$req(nrow(data_plot) > 0)
      setorderv(data_plot, rac)
      data_plot <- head(data_plot, input$top_n)
      data_plot[, active_value := .SD[[1]], .SDcols = mc]
      data_plot[, active_rank := .SD[[1]], .SDcols = rac]
      data_plot[, active_group := .SD[[1]], .SDcols = rc]
      data_plot[, district_label := paste0("#", active_rank, " ", admin_2)]

      y_max <- max(data_plot$active_value, na.rm = TRUE)

      ggplot(
        data_plot,
        aes(
          x = reorder(district_label, -active_rank),
          y = active_value,
          fill = active_group
        )
      ) +
        geom_bar(stat = "identity", alpha = 0.88, width = 0.65) +
        geom_text(
          aes(
            label = if (mc == "impact_per_1000") {
              round(active_value, 1)
            } else {
              format(round(active_value), big.mark = ",")
            }
          ),
          hjust = -0.1,
          size = 4,
          color = "gray20"
        ) +
        scale_fill_manual(
          values = RANK_COLORS,
          breaks = names(RANK_COLORS),
          name = "Priority Rank"
        ) +
        coord_flip() +
        expand_limits(y = y_max * 1.18) +
        labs(
          x = "",
          y = metric_label(),
          subtitle = paste(
            "Top",
            input$top_n,
            "districts -",
            scenario_display_name(),
            "-",
            age_label()
          )
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.subtitle = element_text(color = "gray50", size = 11),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90"),
          legend.position = "right",
          axis.text.y = element_text(size = 11)
        )
    })
  })
}
