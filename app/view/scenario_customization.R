box::use(
  bsicons[bs_icon],
  bslib[
    accordion,
    accordion_panel,
    card,
    card_body,
    card_header,
    input_task_button,
    layout_column_wrap,
  ],
  config,
  data.table[`:=`, rbindlist, setDT],
  logger[log_debug],
  mirai[mirai],
  shiny,
  shinyWidgets,
  rlang[`%||%`]
)

# fmt: skip
box::use(
  app/logic/interventions/combination_matcher,
  app/logic/interventions/intervention_conflicts,
  app/logic/core/logging[log_ns_builder, log_ns_fn],
)

# UI ---------------------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  # Top section: Scenario & Custom Upload
  card(
    card_header("Scenarios & Customization"),
    card_body(
      # Scenario customization module for custom package building
      # Wrapped in accordion for visible clickable styling
      accordion(
        id = ns("accordion_scenario"),
        open = FALSE,
        accordion_panel(
          "Scenario customization",
          shiny$div(
            class = "alert alert-info",
            style = "margin-bottom: 15px;",
            bs_icon("info-circle-fill"),
            " ",
            "Select which districts receive each intervention. "
          ),

          # Base scenario selectors
          shiny$selectInput(
            inputId = ns("selected_base_plan"),
            label = "Base National Plan:",
            choices = "Loading ..."
          ),
          shiny$selectInput(
            inputId = ns("selected_reference"),
            label = "Choose Reference:",
            choices = "Loading ..."
          ),

          # Loading indicator for data pool
          shiny$uiOutput(ns("data_pool_loading_status")),

          # # Intervention selection section
          # shiny$h4("Change Interventions"),

          # Button to open intervention selector modal
          shiny$actionButton(
            ns("open_intervention_selector"),
            "Configure Interventions",
            style = "margin-top: 20px; margin-bottom: 20px;"
          ),

          # Action buttons
          shiny$div(
            style = "margin-top: 20px; margin-bottom: 20px;",
            input_task_button(
              ns("apply_selections"),
              "Apply Selections",
              style = "width: 100%;",
              label_busy = "Applying...",
              icon = bs_icon("check-circle")
            ),
            shiny$actionButton(
              ns("reset_selections"),
              "Reset to Base Plan",
              style = "width: 100%;"
            )
          ),

          # Summary of current selections
          shiny$uiOutput(ns("intervention_summary")),

          # Compact validation summary with modal button
          shiny$uiOutput(ns("validation_summary_compact")),

          # # Debug panel (collapsible)
          # shiny$checkboxInput(ns("show_debug"), "Show Debug Info", FALSE),
          # shiny$conditionalPanel(
          #   condition = "input.show_debug",
          #   ns = ns,
          #   card(
          #     full_screen = TRUE,
          #     card_header("Debug Information"),
          #     card_body(shiny$verbatimTextOutput(ns("debug_output")))
          #   )
          # )
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

#' @export
server <- function(id, variables, trigger, ...) {
  shiny$moduleServer(id, function(input, output, session) {
    args <- list(...)
    log_ns <- log_ns_builder(args, "scenario_customization")

    ## Reactive values ---------------------------------------------------------

    # Base scenario name
    base_scenario_name <- shiny$reactiveVal()

    # Custom scenario and reference data
    custom_scen_data <- shiny$reactiveVal()
    custom_cf_data <- shiny$reactiveVal()

    # Data pools
    int_data_pool <- shiny$reactiveVal()

    # Validation state per district
    district_validation_state <- shiny$reactiveVal(list())

    # Resolved district data (after conflict resolution and best-match)
    resolved_district_data <- shiny$reactiveValues()

    # Loading status indicators
    data_pool_loading <- shiny$reactiveVal(FALSE)

    ## Filtered admins based on aggregation level ------------------------------

    # Reactive to filter districts based on aggregation level
    filtered_admins <- shiny$reactive({
      shiny$req(variables$admins)
      shiny$req(variables$session_state$agg_level)

      admins <- variables$admins
      agg_level <- variables$session_state$agg_level

      if (agg_level == config$get("aggregation_levels")[2]) {
        region_selected <- variables$session_state$region_selected
        if (length(region_selected) > 0) {
          admins <- admins[admin_1 %in% region_selected]
        }
      } else if (agg_level == "Risk Strata") {
        # For risk strata, we need to get districts from int_data_pool
        # since variables$admins doesn't have risk_stratum column
        shiny$req(int_data_pool())
        strata_selected <- variables$session_state$strata_selected
        if (length(strata_selected) > 0) {
          # Get unique districts from the already-filtered int_data_pool
          districts_in_strata <- unique(int_data_pool()$admin_2)
          admins <- admins[admin_2 %in% districts_in_strata]
        }
      }
      # If "National", return all districts (no filtering)

      admins
    })

    ## Initialization ----------------------------------------------------------

    # Initialize plan and reference dropdowns
    shiny$observe({
      shiny$updateSelectInput(
        session = session,
        inputId = "selected_base_plan",
        choices = unname(unlist(config$get(
          "scenario_mapping"
        ))[unlist(config$get("plans"))]),
        selected = unname(unlist(config$get(
          "scenario_mapping"
        ))[unlist(config$get("plans")[["scenarios"]])])[1]
      )

      shiny$updateSelectInput(
        session = session,
        inputId = "selected_reference",
        choices = unname(unlist(config$get(
          "scenario_mapping"
        ))[unlist(config$get("plans"))]),
        selected = unname(unlist(config$get(
          "scenario_mapping"
        ))[unlist(config$get("plans")[["references"]])])[1]
      )
    })

    # Update reference in variables when changed
    shiny$observe({
      variables$set_reference(input$selected_reference)
    }) |>
      shiny$bindEvent(input$selected_reference)

    ## Data retrieval ----------------------------------------------------------

    custom_data_task_fn <- function(...) {
      args <- list(...)
      age_group <- args$age_group
      plan <- args$plan
      scenario_name <- args$scenario_name
      agg_level <- args$agg_level
      region_selected <- args$region_selected
      strata_selected <- args$strata_selected
      data_con <- args$data_con
      conversion_rules <- args$conversion_rules
      log_ns <- log_ns_builder(args, "custom_data_task_fn")

      # Build filters
      filters <- list(age_group = age_group)
      if (exists("plan") && !is.null(plan)) {
        filters <- c(filters, list(plan = plan))
      }
      if (exists("scenario_name") && !is.null(scenario_name)) {
        filters <- c(filters, list(scenario_name = scenario_name))
      }
      if (agg_level == config$get("aggregation_levels")[2] && length(region_selected) > 0) {
        filters <- c(filters, list(admin_1 = region_selected))
      }
      if (agg_level == "Risk Strata" && length(strata_selected) > 0) {
        filters <- c(filters, list(risk_stratum = strata_selected))
      }

      dt <- data_con$get_filtered_data(
        filters = filters,
        conversion_rules = conversion_rules,
        log_ns = log_ns
      )

      dt
    }

    # Fetch custom scenario data
    shiny$observe({
      shiny$req(input$selected_base_plan)
      shiny$req(variables$session_state$age_group)
      shiny$req(variables$session_state$agg_level)
      shiny$req(variables$session_state$region_selected)
      shiny$req(variables$session_state$strata_selected)

      scenario <- names(
        which(variables$scenario_mapping == input$selected_base_plan)
      )
      base_scenario_name(scenario)

      # Also set scenario_selected in session state for modules that depend on it
      # (e.g., intervention_prioritization)
      variables$set_scenario_selected(input$selected_base_plan)

      log_debug(
        paste0(
          "Fetching custom scenario data for: ",
          scenario
        ),
        namespace = log_ns_fn(log_ns)
      )

      custom_scen_data(custom_data_task_fn(
        age_group = variables$session_state$age_group,
        plan = unname(unlist(lapply(config$get("plans"), names))),
        scenario_name = scenario,
        agg_level = variables$session_state$agg_level,
        region_selected = variables$session_state$region_selected,
        strata_selected = variables$session_state$strata_selected,
        data_con = variables$data_con,
        conversion_rules = variables$conversion_rules,
        log_ns = log_ns
      ))
    }) |>
      shiny$bindEvent(
        input$selected_base_plan,
        variables$session_state$age_group,
        variables$session_state$agg_level,
        variables$session_state$region_selected,
        variables$session_state$strata_selected
      )

    # Fetch reference data
    shiny$observe({
      shiny$req(input$selected_reference)
      shiny$req(variables$session_state$age_group)
      shiny$req(variables$session_state$agg_level)
      shiny$req(variables$session_state$region_selected)
      shiny$req(variables$session_state$strata_selected)

      log_debug(
        "Fetching reference data...",
        namespace = log_ns_fn(log_ns)
      )

      custom_cf_data(custom_data_task_fn(
        age_group = variables$session_state$age_group,
        plan = unname(unlist(lapply(config$get("plans"), names))),
        scenario_name = names(
          which(variables$scenario_mapping == input$selected_reference)
        ),
        agg_level = variables$session_state$agg_level,
        region_selected = variables$session_state$region_selected,
        strata_selected = variables$session_state$strata_selected,
        data_con = variables$data_con,
        conversion_rules = variables$conversion_rules,
        log_ns = log_ns
      ))
    }) |>
      shiny$bindEvent(
        input$selected_reference,
        variables$session_state$age_group,
        variables$session_state$agg_level,
        variables$session_state$region_selected,
        variables$session_state$strata_selected
      )

    # Fetch int data pool (intervention combinations)
    int_data_pool_task_fn <- function(...) {
      args <- list(...)
      age_group <- args$age_group
      intervention_cols <- args$intervention_cols
      agg_level <- args$agg_level
      region_selected <- args$region_selected
      strata_selected <- args$strata_selected
      data_con <- args$data_con
      conversion_rules <- args$conversion_rules
      log_ns <- log_ns_builder(args, "int_data_pool_task_fn")

      # Build filters
      filters <- list(
        age_group = age_group,
        year = variables$session_state$year_end
      )
      if (agg_level == config$get("aggregation_levels")[2] && length(region_selected) > 0) {
        filters <- c(filters, list(admin_1 = region_selected))
      }
      if (agg_level == "Risk Strata" && length(strata_selected) > 0) {
        filters <- c(filters, list(risk_stratum = strata_selected))
      }

      dt <- data_con$get_filtered_data(
        source = "int_data_pool",
        columns = c("scenario_name", intervention_cols, "admin_2"),
        filters = filters,
        conversion_rules = conversion_rules,
        log_ns = log_ns
      )

      dt
    }

    shiny$observe({
      shiny$req(variables$session_state$age_group)
      shiny$req(variables$intervention_cols)
      shiny$req(variables$session_state$agg_level)
      shiny$req(variables$session_state$region_selected)
      shiny$req(variables$session_state$strata_selected)

      log_debug(
        "Fetching intervention data pool...",
        namespace = log_ns_fn(log_ns)
      )

      # Mark pool as not ready while fetching
      variables$set_int_data_pool_ready(FALSE)

      # Set loading indicator
      data_pool_loading(TRUE)

      # Fetch data
      pool_data <- int_data_pool_task_fn(
        age_group = variables$session_state$age_group,
        intervention_cols = variables$intervention_cols,
        agg_level = variables$session_state$agg_level,
        region_selected = variables$session_state$region_selected,
        strata_selected = variables$session_state$strata_selected,
        data_con = variables$data_con,
        conversion_rules = variables$conversion_rules,
        log_ns = log_ns
      )

      int_data_pool(pool_data)

      # Mark pool as ready
      variables$set_int_data_pool_ready(TRUE)

      # Clear loading indicator
      data_pool_loading(FALSE)

      log_debug(
        paste0(
          "Intervention data pool loaded: ",
          nrow(pool_data),
          " rows"
        ),
        namespace = log_ns_fn(log_ns)
      )
    }) |>
      shiny$bindEvent(
        variables$session_state$age_group,
        variables$intervention_cols,
        variables$session_state$agg_level,
        variables$session_state$region_selected,
        variables$session_state$strata_selected
      )

    ## Initialize intervention mapping from base plan --------------------------

    shiny$observe({
      shiny$req(input$selected_base_plan, int_data_pool(), variables$admins)

      scenario <- base_scenario_name()

      log_debug(
        paste0(
          "Initializing intervention mapping from base plan: ",
          scenario
        ),
        namespace = log_ns_fn(log_ns)
      )

      # Build initial mapping from base scenario
      initial_mapping <- list()

      for (int_col in variables$intervention_cols) {
        # Find districts where this intervention is deployed in base plan
        districts_with_int <- int_data_pool()[
          scenario_name == scenario & get(int_col) == TRUE,
          admin_2
        ]

        initial_mapping[[int_col]] <- districts_with_int
      }

      variables$set_intervention_district_mapping(initial_mapping)

      log_debug(
        paste0(
          "Initialized with ",
          length(initial_mapping),
          " interventions"
        ),
        namespace = log_ns_fn(log_ns)
      )
    }) |>
      shiny$bindEvent(input$selected_base_plan, int_data_pool())

    ## UI - Intervention Selectors Modal ---------------------------------------

    # Observer to show modal when button clicked
    shiny$observe({
      ns <- session$ns

      shiny$showModal(
        shiny$modalDialog(
          title = "Configure Intervention Assignments",
          size = "xl",
          easyClose = FALSE,
          footer = shiny$tagList(
            shiny$actionButton(
              ns("modal_close"),
              "Close",
              class = "btn-secondary"
            ),
            shiny$actionButton(
              ns("modal_apply"),
              "Apply Changes",
              class = "btn-primary"
            )
          ),
          shiny$div(
            style = "max-height: 70vh; overflow-y: auto; padding: 10px;",
            shiny$div(
              style = "margin-bottom: 20px;",
              shiny$p(
                style = "color: #666; margin-bottom: 15px;",
                "Select which districts receive each intervention. ",
                "Districts not selected for any intervention will use the base plan. ",
                "Use the region groupings to select multiple districts at once."
              ),
              shiny$actionButton(
                ns("clear_all_interventions"),
                "Clear All Interventions",
                icon = shiny$icon("times-circle"),
                class = "btn-warning"
              )
            ),
            shiny$uiOutput(ns("intervention_selectors"))
          )
        )
      )
    }) |>
      shiny$bindEvent(input$open_intervention_selector)

    # Close modal on close button
    shiny$observe({
      shiny$removeModal()
    }) |>
      shiny$bindEvent(input$modal_close)

    # Close modal on apply button
    shiny$observe({
      shiny$removeModal()
      shiny$showNotification(
        "Intervention assignments updated. Click 'Apply Selections' to use them.",
        type = "message",
        duration = 3
      )
    }) |>
      shiny$bindEvent(input$modal_apply)

    # Helper function to prepare grouped district choices
    prepare_grouped_districts <- function(admins_dt) {
      regions <- unique(admins_dt$admin_1)
      result <- lapply(regions, function(region) {
        admins_dt[admin_1 == region, admin_2]
      })
      names(result) <- regions
      result
    }

    # Render intervention selectors (used in modal)
    output$intervention_selectors <- shiny$renderUI({
      ns <- session$ns
      shiny$req(filtered_admins(), variables$intervention_cols)
      shiny$req(variables$session_state$intervention_district_mapping)

      # Prepare district choices grouped by region
      district_choices <- prepare_grouped_districts(filtered_admins())

      # Create one virtualSelectInput per intervention with clear button
      intervention_uis <- lapply(
        variables$intervention_cols,
        function(int_col) {
          int_name <- gsub("^deployed_int_", "", int_col)
          current_selected <-
            variables$session_state$intervention_district_mapping[[int_col]]

          shiny$div(
            style = "margin-bottom: 20px;",
            # Header with intervention name and clear button
            shiny$div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
              shiny$h5(
                int_name,
                style = "margin: 0; font-weight: 600;"
              )
            ),
            shinyWidgets$virtualSelectInput(
              inputId = ns(paste0("districts_", int_col)),
              label = NULL,
              choices = district_choices,
              selected = current_selected,
              multiple = TRUE,
              search = TRUE,
              optionsCount = 10,
              showValueAsTags = TRUE,
              updateOn = "close",
              noOfDisplayValues = 2,
              placeholder = paste0("Select districts for ", int_name, "...")
            )
          )
        }
      )

      # Wrap in responsive grid layout
      layout_column_wrap(
        width = "250px", # Minimum column width
        !!!intervention_uis
      )
    }) |>
      shiny$bindEvent(
        variables$session_state$intervention_district_mapping,
        filtered_admins(),
        ignoreNULL = FALSE
      )

    ## Data pool loading status ------------------------------------------------

    output$data_pool_loading_status <- shiny$renderUI({
      if (isTRUE(data_pool_loading())) {
        shiny$div(
          style = "padding: 10px; margin: 15px 0; text-align: center; background-color: #f8f9fa; border-radius: 5px;",
          shiny$div(
            class = "spinner-border spinner-border-sm text-primary",
            role = "status",
            style = "margin-right: 10px;"
          ),
          shiny$span(
            "Loading intervention data...",
            style = "color: #666;"
          )
        )
      }
    })

    ## Intervention summary ----------------------------------------------------

    output$intervention_summary <- shiny$renderUI({
      shiny$req(variables$session_state$intervention_district_mapping)
      shiny$req(filtered_admins())

      mapping <- variables$session_state$intervention_district_mapping
      filtered_district_names <- filtered_admins()$admin_2

      # Count total districts with interventions (within filtered set)
      all_selected_districts <- unique(unlist(mapping))
      # Only count selected districts that are in the filtered set
      all_selected_districts <- all_selected_districts[
        all_selected_districts %in% filtered_district_names
      ]
      n_districts_with_interventions <- length(all_selected_districts)
      n_total_districts <- length(filtered_district_names)

      # Count interventions with selections
      n_interventions_used <- sum(sapply(mapping, function(x) length(x) > 0))

      card(
        card_header("Current Intervention Assignments"),
        card_body(
          style = "padding: 15px;",
          shiny$p(
            style = "margin-bottom: 10px;",
            shiny$strong(n_districts_with_interventions),
            " of ",
            n_total_districts,
            " districts have interventions assigned"
          ),
          shiny$p(
            style = "margin-bottom: 15px;",
            shiny$strong(n_interventions_used),
            " of ",
            length(mapping),
            " interventions are in use"
          )
        )
      )
    })

    ## Clear functionality -----------------------------------------------------

    # Clear all interventions
    shiny$observe({
      # Reset all interventions to empty
      current_mapping <- variables$session_state$intervention_district_mapping
      for (int_col in names(current_mapping)) {
        current_mapping[[int_col]] <- character(0)
      }
      variables$set_intervention_district_mapping(current_mapping)

      shiny$showNotification(
        paste(
          "All interventions cleared.",
          "Will look for 'no intervention' scenario when applied."
        ),
        type = "message",
        duration = 4
      )
    }) |>
      shiny$bindEvent(input$clear_all_interventions)

    # Create clear observers for each intervention
    shiny$observe({
      shiny$req(variables$intervention_cols)

      lapply(variables$intervention_cols, function(int_col) {
        clear_button_id <- paste0("clear_", int_col)

        shiny$observe({
          # Clear this specific intervention
          current_mapping <-
            variables$session_state$intervention_district_mapping
          current_mapping[[int_col]] <- character(0)
          variables$set_intervention_district_mapping(current_mapping)

          int_name <- gsub("^deployed_int_", "", int_col)
          shiny$showNotification(
            paste0(int_name, " cleared"),
            type = "message",
            duration = 2
          )
        }) |>
          shiny$bindEvent(input[[clear_button_id]])
      })
    }) |>
      shiny$bindEvent(variables$intervention_cols, once = TRUE)

    ## Capture user selections -------------------------------------------------

    # Create observers for each intervention selector
    shiny$observe({
      shiny$req(variables$intervention_cols)

      lapply(variables$intervention_cols, function(int_col) {
        input_id <- paste0("districts_", int_col)

        shiny$observe({
          selected_districts <- input[[input_id]]

          # Update mapping
          current_mapping <-
            variables$session_state$intervention_district_mapping
          current_mapping[[int_col]] <- selected_districts %||% character(0)
          variables$set_intervention_district_mapping(current_mapping)
        }) |>
          shiny$bindEvent(input[[input_id]], ignoreNULL = FALSE)
      })
    }) |>
      shiny$bindEvent(variables$intervention_cols, once = TRUE)

    ## Validation and conflict resolution --------------------------------------

    # Main validation logic
    # We are using debounce here to keep the UI a bit more responsive
    validation_trigger <- shiny$debounce(
      shiny$reactive(
        variables$session_state$intervention_district_mapping
      ),
      millis = 1000
    )

    # Async validation task function
    validation_task <- shiny$ExtendedTask$new(function(
      mapping,
      all_districts,
      priorities,
      intervention_cols,
      int_data_pool_dt,
      threshold,
      log_ns
    ) {
      mirai(
        {
          options(box.path = getwd())

          box::use(
            logger[
              ERROR,
              layout_glue_generator,
              log_debug,
              log_layout,
              log_threshold,
            ],
            rlang[`%||%`],
          )

          # fmt: skip
          box::use(
            app/logic/interventions/combination_matcher,
            app/logic/interventions/intervention_conflicts,
            app/logic/core/logging[log_ns_builder, log_ns_fn],
          )

          # Setup logging for the individual daemons
          log_ns <- log_ns_builder(list(log_ns = log_ns), "validation_task")

          log_threshold(threshold)
          log_threshold(ERROR, namespace = "__gutter")
          logger_layout <- layout_glue_generator(
            format = "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")} \\
daemon:{Sys.getpid()}] {ns}: {msg}"
          )
          log_layout(logger_layout)

          log_debug(
            paste0(
              "Starting async validation for ",
              length(all_districts),
              " filtered districts..."
            ),
            namespace = log_ns_fn(log_ns)
          )

          validation_results <- list()
          resolved_data_list <- list()

          for (district in all_districts) {
            # Determine which interventions are selected for this district
            selected_ints <- character(0)
            for (int_col in names(mapping)) {
              if (district %in% mapping[[int_col]]) {
                selected_ints <- c(selected_ints, int_col)
              }
            }

            # Detect conflicts (only if interventions selected)
            resolved_ints <- selected_ints
            if (length(selected_ints) > 0) {
              conflicts <- intervention_conflicts$detect_conflicts(
                selected_ints,
                district,
                log_ns = log_ns
              )

              # Resolve conflicts if present
              if (conflicts$has_conflict) {
                resolved_ints <- intervention_conflicts$resolve_conflicts(
                  selected_ints,
                  conflicts,
                  priorities,
                  log_ns = log_ns
                )

                validation_results[[district]] <- list(
                  has_conflict = TRUE,
                  conflict_type = paste(
                    names(conflicts$conflict_groups),
                    collapse = ", "
                  ),
                  details = paste(conflicts$details, collapse = "; "),
                  original_selection = selected_ints,
                  resolved_selection = resolved_ints
                )
              } else {
                validation_results[[district]] <- list(
                  has_conflict = FALSE
                )
              }
            } else {
              # No interventions selected
              validation_results[[district]] <- list(
                has_conflict = FALSE,
                no_interventions_requested = TRUE
              )
            }

            # Create target intervention vector
            target_vector <- combination_matcher$create_intervention_list(
              resolved_ints,
              intervention_cols
            )

            # Find best match
            match_result <- combination_matcher$find_best_match(
              district_name = district,
              target_interventions = target_vector,
              int_data_pool_dt = int_data_pool_dt,
              priority_weights = priorities,
              log_ns = log_ns
            )

            # Record match info
            if (!match_result$exact_match) {
              validation_results[[district]]$has_fallback <- TRUE
              validation_results[[
                district
              ]]$fallback_type <- "missing_combination"
              validation_results[[district]]$hamming_distance <-
                match_result$distance

              # Handle original combo display
              if (length(resolved_ints) == 0) {
                validation_results[[
                  district
                ]]$original_combo <- "No interventions"
              } else {
                validation_results[[district]]$original_combo <- paste(
                  gsub("^deployed_int_", "", resolved_ints),
                  collapse = " + "
                )
              }

              # Handle matched combo display
              matched_int_names <- names(match_result$matched_interventions)[
                unlist(match_result$matched_interventions)
              ]
              if (length(matched_int_names) == 0) {
                validation_results[[
                  district
                ]]$matched_combo <- "No interventions"
              } else {
                validation_results[[district]]$matched_combo <- paste(
                  gsub("^deployed_int_", "", matched_int_names),
                  collapse = " + "
                )
              }
            } else if (length(resolved_ints) == 0) {
              # Exact match for "no interventions"
              validation_results[[
                district
              ]]$no_interventions_exact_match <- TRUE
            }

            # Store matched data for this district
            resolved_data_list[[district]] <- match_result$matched_data
          }

          log_debug(
            paste0(
              "Validation complete. Conflicts: ",
              sum(sapply(validation_results, function(x) {
                x$has_conflict %||% FALSE
              })),
              ", Fallbacks: ",
              sum(sapply(validation_results, function(x) {
                x$has_fallback %||% FALSE
              }))
            ),
            namespace = log_ns_fn(log_ns)
          )

          # Return results
          list(
            validation_results = validation_results,
            resolved_data_list = resolved_data_list
          )
        },
        mapping = mapping,
        all_districts = all_districts,
        priorities = priorities,
        intervention_cols = intervention_cols,
        int_data_pool_dt = int_data_pool_dt,
        threshold = threshold,
        log_ns = log_ns
      )
    })

    # Trigger async validation task
    shiny$observe({
      shiny$req(validation_trigger())
      shiny$req(int_data_pool())
      shiny$req(filtered_admins())

      mapping <- variables$session_state$intervention_district_mapping
      all_districts <- unique(filtered_admins()$admin_2)
      priorities <- intervention_conflicts$get_intervention_priorities()

      log_debug(
        paste0(
          "Triggering async validation for ",
          length(all_districts),
          " filtered districts..."
        ),
        namespace = log_ns_fn(log_ns)
      )

      # Get logger threshold to pass to daemon
      threshold <- logger::log_threshold()

      # Invoke async task
      validation_task$invoke(
        mapping = mapping,
        all_districts = all_districts,
        priorities = priorities,
        intervention_cols = variables$intervention_cols,
        int_data_pool_dt = int_data_pool(),
        threshold = threshold,
        log_ns = log_ns
      )
    }) |>
      shiny$bindEvent(validation_trigger())

    # Handle validation results
    shiny$observe({
      result <- validation_task$result()
      shiny$req(result)

      # Check for errors
      if (inherits(result, "error") || inherits(result, "miraiError")) {
        log_debug(
          paste0("Validation task failed: ", conditionMessage(result)),
          namespace = log_ns_fn(log_ns)
        )
        shiny$showNotification(
          paste0("Validation failed: ", conditionMessage(result)),
          type = "error",
          duration = 10
        )
        return()
      }

      log_debug(
        "Received validation results from async task",
        namespace = log_ns_fn(log_ns)
      )

      # Extract results
      validation_results <- result$validation_results
      resolved_data_list <- result$resolved_data_list

      # Store validation results
      district_validation_state(validation_results)

      # Store resolved district data
      for (district in names(resolved_data_list)) {
        resolved_district_data[[district]] <- resolved_data_list[[district]]
      }

      log_debug(
        paste0(
          "Validation results stored. Conflicts: ",
          sum(sapply(validation_results, function(x) {
            x$has_conflict %||% FALSE
          })),
          ", Fallbacks: ",
          sum(sapply(validation_results, function(x) x$has_fallback %||% FALSE))
        ),
        namespace = log_ns_fn(log_ns)
      )
    }) |>
      shiny$bindEvent(validation_task$result())

    ## UI - Compact Validation Summary with Modal ------------------------------

    # Compact validation summary (icons + numbers only)
    output$validation_summary_compact <- shiny$renderUI({
      # Make reactive to task status
      task_status <- validation_task$status()

      # Check if async task is running
      if (isTRUE(task_status == "running")) {
        return(
          card(
            card_header("Validation Status"),
            card_body(
              style = "text-align: center; padding: 20px;",
              shiny$div(
                class = "spinner-border text-primary",
                role = "status",
                shiny$span(class = "visually-hidden", "Validating...")
              ),
              shiny$p(
                "Validating selections ...",
                style = "margin-top: 10px; color: #666;"
              )
            )
          )
        )
      }

      validation <- district_validation_state()

      # Show loading indicator if validation not ready
      if (is.null(validation) || length(validation) == 0) {
        return(
          card(
            card_header("Validation Status"),
            card_body(
              style = "text-align: center; padding: 20px;",
              shiny$div(
                class = "spinner-border text-primary",
                role = "status",
                shiny$span(class = "visually-hidden", "Validating...")
              ),
              shiny$p(
                "Waiting for validation...",
                style = "margin-top: 10px; color: #666;"
              )
            )
          )
        )
      }

      n_conflicts <- sum(sapply(
        validation,
        function(x) x$has_conflict %||% FALSE
      ))
      n_fallbacks <- sum(sapply(
        validation,
        function(x) x$has_fallback %||% FALSE
      ))
      n_valid <- length(validation) - n_conflicts - n_fallbacks

      card(
        card_header("Validation Status"),
        card_body(
          style = "padding: 15px;",
          # Compact icon row
          shiny$div(
            style = "display: flex; justify-content: space-around; margin-bottom: 15px;",
            # Valid
            shiny$div(
              style = "text-align: center; cursor: pointer;",
              onclick = paste0(
                "Shiny.setInputValue('",
                session$ns("show_validation_details"),
                "', Math.random())"
              ),
              bs_icon(
                "check-circle-fill",
                class = "text-success",
                size = "1.5em"
              ),
              shiny$div(
                shiny$strong(n_valid),
                style = "margin-top: 5px; font-size: 1.2em;"
              ),
              shiny$div("Valid", style = "font-size: 0.8em; color: #666;")
            ),
            # Fallbacks
            shiny$div(
              style = "text-align: center; cursor: pointer;",
              onclick = paste0(
                "Shiny.setInputValue('",
                session$ns("show_validation_details"),
                "', Math.random())"
              ),
              bs_icon(
                "exclamation-triangle-fill",
                class = "text-warning",
                size = "1.5em"
              ),
              shiny$div(
                shiny$strong(n_fallbacks),
                style = "margin-top: 5px; font-size: 1.2em;"
              ),
              shiny$div("Fallback", style = "font-size: 0.8em; color: #666;")
            ),
            # Conflicts
            shiny$div(
              style = "text-align: center; cursor: pointer;",
              onclick = paste0(
                "Shiny.setInputValue('",
                session$ns("show_validation_details"),
                "', Math.random())"
              ),
              bs_icon("x-circle-fill", class = "text-danger", size = "1.5em"),
              shiny$div(
                shiny$strong(n_conflicts),
                style = "margin-top: 5px; font-size: 1.2em;"
              ),
              shiny$div("Conflict", style = "font-size: 0.8em; color: #666;")
            )
          ),
          # Button to show details
          shiny$actionButton(
            session$ns("show_validation_details"),
            "View Detailed Report",
            icon = shiny$icon("info-circle"),
            class = "btn-sm btn-outline-primary",
            style = "width: 100%;"
          )
        )
      )
    })

    # Observer to show validation details modal
    shiny$observe({
      ns <- session$ns
      validation <- district_validation_state()
      shiny$req(validation)

      n_conflicts <- sum(sapply(
        validation,
        function(x) x$has_conflict %||% FALSE
      ))
      n_fallbacks <- sum(sapply(
        validation,
        function(x) x$has_fallback %||% FALSE
      ))
      n_valid <- length(validation) - n_conflicts - n_fallbacks

      shiny$showModal(
        shiny$modalDialog(
          title = "Validation Report",
          size = "xl",
          easyClose = TRUE,
          footer = shiny$actionButton(
            ns("close_validation_modal"),
            "Close",
            class = "btn-primary"
          ),
          # Full validation summary
          shiny$div(
            style = "padding: 10px;",
            shiny$h4("Summary"),
            shiny$fluidRow(
              shiny$column(
                width = 4,
                shiny$div(
                  class = "text-center",
                  style = "padding: 20px; border: 1px solid #dee2e6; border-radius: 5px;",
                  bs_icon(
                    "check-circle-fill",
                    class = "text-success",
                    size = "3em"
                  ),
                  shiny$h3(n_valid, style = "margin-top: 10px;"),
                  shiny$p("Valid Districts", style = "color: #666;")
                )
              ),
              shiny$column(
                width = 4,
                shiny$div(
                  class = "text-center",
                  style = "padding: 20px; border: 1px solid #dee2e6; border-radius: 5px;",
                  bs_icon(
                    "exclamation-triangle-fill",
                    class = "text-warning",
                    size = "3em"
                  ),
                  shiny$h3(n_fallbacks, style = "margin-top: 10px;"),
                  shiny$p("Best-Match Used", style = "color: #666;")
                )
              ),
              shiny$column(
                width = 4,
                shiny$div(
                  class = "text-center",
                  style = "padding: 20px; border: 1px solid #dee2e6; border-radius: 5px;",
                  bs_icon(
                    "x-circle-fill",
                    class = "text-danger",
                    size = "3em"
                  ),
                  shiny$h3(n_conflicts, style = "margin-top: 10px;"),
                  shiny$p("Conflicts Resolved", style = "color: #666;")
                )
              )
            ),
            shiny$hr(),
            # Conflict details
            shiny$uiOutput(ns("validation_modal_details"))
          )
        )
      )
    }) |>
      shiny$bindEvent(input$show_validation_details)

    # Close validation modal
    shiny$observe({
      shiny$removeModal()
    }) |>
      shiny$bindEvent(input$close_validation_modal)

    # Render conflict details in modal
    output$validation_modal_details <- shiny$renderUI({
      validation <- district_validation_state()
      shiny$req(validation)

      # Filter to only districts with issues
      issues <- Filter(
        function(x) {
          (x$has_conflict %||% FALSE) || (x$has_fallback %||% FALSE)
        },
        validation
      )

      if (length(issues) == 0) {
        return(shiny$div(
          class = "alert alert-success",
          style = "margin-top: 15px;",
          bs_icon("check-circle-fill"),
          " All district selections are valid!"
        ))
      }

      shiny$div(
        shiny$h4(
          paste0("Issues Found (", length(issues), " districts)")
        ),
        shiny$div(
          style = "max-height: 400px; overflow-y: auto; margin-top: 15px;",
          lapply(names(issues), function(district_name) {
            issue <- issues[[district_name]]

            if (issue$has_conflict) {
              shiny$div(
                class = "alert alert-danger",
                style = "margin-bottom: 10px;",
                shiny$strong(district_name, ": Conflict Resolved"),
                shiny$p(issue$details, style = "margin: 5px 0;"),
                shiny$p(
                  style = "margin: 0; font-size: 0.9em;",
                  "Original: ",
                  paste(
                    gsub("^deployed_int_", "", issue$original_selection),
                    collapse = ", "
                  ),
                  shiny$br(),
                  "Resolved: ",
                  paste(
                    gsub("^deployed_int_", "", issue$resolved_selection),
                    collapse = ", "
                  )
                )
              )
            } else if (issue$has_fallback) {
              shiny$div(
                class = "alert alert-warning",
                style = "margin-bottom: 10px;",
                shiny$strong(district_name, ": Best Match Used"),
                shiny$p(
                  style = "margin: 5px 0;",
                  "Exact combination not available. ",
                  "Distance: ",
                  issue$hamming_distance,
                  " intervention(s) differ"
                ),
                shiny$p(
                  style = "margin: 0; font-size: 0.9em;",
                  "Requested: ",
                  issue$original_combo %||% "N/A",
                  shiny$br(),
                  "Matched: ",
                  issue$matched_combo %||% "N/A"
                )
              )
            }
          })
        )
      )
    })

    ## Data assembly on Apply --------------------------------------------------

    shiny$observe({
      shiny$req(variables$session_state$age_group)
      shiny$req(variables$session_state$agg_level)
      shiny$req(variables$session_state$region_selected)
      shiny$req(variables$session_state$strata_selected)
      shiny$req(filtered_admins())

      # Check if validation task is still running
      # This change was introduced due to incorrect intervention display
      task_status <- validation_task$status()
      if (isTRUE(task_status == "running")) {
        shiny$showNotification(
          "Please wait for validation to complete before applying.",
          type = "warning",
          duration = 3
        )
        return()
      }

      # Check if we have validation results for the filtered districts
      all_districts <- unique(filtered_admins()$admin_2)
      validation_state <- district_validation_state()
      if (
        length(validation_state) == 0 ||
          !all(all_districts %in% names(validation_state))
      ) {
        shiny$showNotification(
          "Validation not yet complete. Please wait a moment and try again.",
          type = "warning",
          duration = 3
        )
        return()
      }

      log_debug(
        "Assembling custom scenario data...",
        namespace = log_ns_fn(log_ns)
      )

      # Show progress notification
      progress_notification <- shiny$showNotification(
        "Assembling custom scenario data...",
        type = "message",
        duration = NULL, # Keep until dismissed
        closeButton = FALSE
      )

      # STEP 1: Collect unique scenario names needed
      required_scenarios <- character(0)

      for (district in all_districts) {
        district_combo <- resolved_district_data[[district]]

        if (is.null(district_combo) || nrow(district_combo) == 0) {
          # Will use base scenario
          required_scenarios <- c(required_scenarios, base_scenario_name())
        } else {
          required_scenarios <- c(
            required_scenarios,
            district_combo[["scenario_name"]]
          )
        }
      }

      required_scenarios <- unique(required_scenarios)

      log_debug(
        paste0(
          "Fetching data for ",
          length(required_scenarios),
          " unique scenarios (out of 386 total)..."
        ),
        namespace = log_ns_fn(log_ns)
      )

      # Update progress notification
      shiny$removeNotification(progress_notification)
      progress_notification <- shiny$showNotification(
        paste0(
          "Fetching data for ",
          length(required_scenarios),
          " scenarios from database..."
        ),
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )

      # STEP 2: Fetch only required scenarios from database (OPTIMIZED)
      custom_data_pool <- custom_data_task_fn(
        age_group = variables$session_state$age_group,
        scenario_name = required_scenarios, # Only fetch what we need!
        agg_level = variables$session_state$agg_level,
        region_selected = variables$session_state$region_selected,
        strata_selected = variables$session_state$strata_selected,
        data_con = variables$data_con,
        conversion_rules = variables$conversion_rules,
        log_ns = log_ns
      )

      log_debug(
        paste0(
          "Fetched ",
          nrow(custom_data_pool),
          " rows (vs ~",
          nrow(custom_data_pool) * (386 / length(required_scenarios)),
          " if all scenarios)"
        ),
        namespace = log_ns_fn(log_ns)
      )

      # Update progress notification
      shiny$removeNotification(progress_notification)
      progress_notification <- shiny$showNotification(
        paste0(
          "Building custom data for ",
          length(all_districts),
          " districts..."
        ),
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )

      # STEP 3: Build final custom scenario data
      new_custom_data <- NULL

      for (district in all_districts) {
        district_combo <- resolved_district_data[[district]]

        if (is.null(district_combo) || nrow(district_combo) == 0) {
          # Fallback to base scenario
          district_combo <- int_data_pool()[
            scenario_name == base_scenario_name() & admin_2 == district
          ][1]
        }

        scenario_name <- district_combo[["scenario_name"]]

        # Get full data for this district from optimized pool
        district_full_data <- custom_data_pool[
          admin_2 == district & scenario_name == scen,
          env = list(scen = I(scenario_name))
        ]

        # Append to custom data
        if (is.null(new_custom_data)) {
          new_custom_data <- district_full_data
        } else {
          new_custom_data <- rbindlist(list(
            new_custom_data,
            district_full_data
          ))
        }
      }

      # Update custom scenario data
      custom_scen_data(new_custom_data)

      log_debug(
        paste0(
          "Custom scenario data assembled: ",
          nrow(new_custom_data),
          " rows"
        ),
        namespace = log_ns_fn(log_ns)
      )

      # Remove progress notification
      shiny$removeNotification(progress_notification)

      # Show success notification
      shiny$showNotification(
        "Custom scenario applied successfully!",
        type = "message",
        duration = 3
      )
    }) |>
      shiny$bindEvent(input$apply_selections)

    ## Reset functionality -----------------------------------------------------

    shiny$observe({
      shiny$req(input$selected_base_plan, int_data_pool())

      scenario <- base_scenario_name()

      # Reset to base plan selections
      initial_mapping <- list()
      for (int_col in variables$intervention_cols) {
        districts_with_int <- int_data_pool()[
          scenario_name == scenario & get(int_col) == TRUE,
          admin_2
        ]
        initial_mapping[[int_col]] <- districts_with_int
      }

      variables$set_intervention_district_mapping(initial_mapping)

      shiny$showNotification(
        "Reset to base plan selections",
        type = "message",
        duration = 3
      )
    }) |>
      shiny$bindEvent(input$reset_selections)

    ## Combine custom scenario and reference ------------------------------

    shiny$observe({
      shiny$req(custom_scen_data(), custom_cf_data())
      shiny$req(nrow(custom_scen_data()) > 0)
      shiny$req(nrow(custom_cf_data()) > 0)

      cf_data <- custom_cf_data()
      custom_scenario_data <- setDT(
        custom_scen_data()
      )[, `:=`(scenario_name = "custom")]

      full <- rbindlist(list(cf_data, custom_scenario_data))
      variables$set_custom_data(full)
      variables$custom_data_run_trigger()

      log_debug(
        "Custom data combined and trigger invoked",
        namespace = log_ns_fn(log_ns)
      )
    }) |>
      shiny$bindEvent(
        custom_scen_data(),
        custom_cf_data()
      )

    ## Debug output ------------------------------------------------------------

    output$debug_output <- shiny$renderPrint({
      cat("=== Intervention-First Customization Debug ===\n\n")

      cat("Base Plan:", input$selected_base_plan, "\n")
      cat("Base Scenario Name:", base_scenario_name(), "\n")
      cat("Reference:", input$selected_reference, "\n\n")

      cat("--- Intervention District Mapping ---\n")
      mapping <- variables$session_state$intervention_district_mapping
      if (length(mapping) > 0) {
        for (int in names(mapping)) {
          cat(
            gsub("^deployed_int_", "", int),
            ": ",
            length(mapping[[int]]),
            " districts\n",
            sep = ""
          )
        }
      } else {
        cat("No mapping yet\n")
      }

      cat("\n--- Validation Summary ---\n")
      validation <- district_validation_state()
      if (length(validation) > 0) {
        n_conflicts <- sum(sapply(
          validation,
          function(x) x$has_conflict %||% FALSE
        ))
        n_fallbacks <- sum(sapply(
          validation,
          function(x) x$has_fallback %||% FALSE
        ))
        cat("Total Districts:", length(validation), "\n")
        cat("Conflicts:", n_conflicts, "\n")
        cat("Fallbacks:", n_fallbacks, "\n")
        cat("Valid:", length(validation) - n_conflicts - n_fallbacks, "\n")
      } else {
        cat("No validation yet\n")
      }

      cat("\n--- Data Pool Status ---\n")
      cat(
        "int_data_pool rows:",
        nrow(int_data_pool() %||% data.frame()),
        "\n"
      )
      cat(
        "custom_scen_data rows:",
        nrow(custom_scen_data() %||% data.frame()),
        "\n"
      )
      cat(
        "custom_cf_data rows:",
        nrow(custom_cf_data() %||% data.frame()),
        "\n"
      )
      cat("\nNOTE: custom_data_pool is now fetched lazily on Apply\n")
      cat("      (only required scenarios, not all 386)\n")
    })
  })
}
