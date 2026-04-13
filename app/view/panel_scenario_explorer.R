box::use(
  bslib,
  config,
  data.table[`%chin%`, merge.data.table],
  logger[log_debug],
  sf[st_as_sf],
  shiny,
  shinyjs,
  shinyWidgets,
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/view/costs,
  app/view/economic_evaluation,
  app/view/impact_map,
  app/view/intervention_map,
  app/view/intervention_prioritization,
  app/view/relative_impact,
  app/view/relative_impact_table,
  app/view/scenario_customization,
  app/view/scenario_picker_simple,
  app/view/static_intervention_map,
  app/view/strata_analysis,
  app/view/value_boxes,
  app/view/yearly_metric,
  app/view/yearly_metric_reduction,
)

# UI ---------------------------------------------------------------------------

#' @export
ui <- function(id, panel_config) {
  ns <- shiny$NS(id)
  bslib$nav_panel(
    panel_config$title,

    ## User input --------------------------------------------------------------

    bslib$layout_sidebar(
      # User input is handled in the side bar
      sidebar = bslib$sidebar(
        if (panel_config$features$scenario_customizer$enabled) {
          scenario_customization$ui(id = ns("scenario_customization"))
        } else {
          scenario_picker_simple$ui(id = ns("scenario_picker_simple"))
        },

        # Bottom section: Filters & Metrics
        bslib$card(
          bslib$card_header("Filters"),
          bslib$card_body(
            # Aggregation level
            shiny$selectInput(
              inputId = ns("agg_level"),
              label = "Choose aggregation level:",
              choices = config$get("aggregation_levels"),
              selected = "National"
            ),

            # District/region selection. Only shown if aggregation level is
            # regional.
            shiny$conditionalPanel(
              condition = "input.agg_level == 'Regional'",
              ns = ns,
              shinyWidgets$pickerInput(
                inputId = ns("region_selected"),
                label = "Select which region to aggregate results for",
                choices = c(),
                multiple = TRUE,
                options = shinyWidgets$pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  selectAllText = "Select all",
                  deselectAllText = "Clear",
                  size = 10
                )
              )
            ),
            # Risk strata selection. Only shown if aggregation level is
            # risk strata.
            shiny$conditionalPanel(
              condition = "input.agg_level == 'Risk Strata'",
              ns = ns,
              shinyWidgets$pickerInput(
                inputId = ns("strata_selected"),
                label = "Select which risk strata to aggregate results for",
                choices = c(),
                multiple = TRUE,
                options = shinyWidgets$pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  selectAllText = "Select all",
                  deselectAllText = "Clear",
                  size = 10
                )
              )
            ),

            # Selection of the age group
            shiny$selectInput(
              inputId = ns("age_group"),
              label = "Choose age group:",
              choices = character(0)
            ),

            # Selection of the end year (start year is locked to minimum)
            shiny$sliderInput(
              inputId = ns("year_end"),
              label = "Select end year:",
              min = 2026,
              max = 2030,
              value = 2030,
              step = 1,
              sep = "",
              ticks = TRUE
            ),

            ### Costs selection ------------------------------------------------

            bslib$accordion(
              id = ns("accordion_costs"),
              open = FALSE,
              bslib$accordion_panel(
                "Define intervention costs (in USD):",
                shiny$tags$small(
                  style = "font-size: 0.85em; opacity: 0.7;",
                  "per person covered"
                ),
                lapply(names(config$get("cost_defaults")), function(name) {
                  shiny$numericInput(
                    ns(paste0("cost_", tolower(name))),
                    paste0(name, ":"),
                    value = config$get("cost_defaults")[[name]],
                    min = 0
                  )
                }),
                # Reset button
                shiny$actionButton(ns("reset_costs"), "Reset costs")
              )
            )
          )
        ),

        # Update button
        bslib$input_task_button(id = ns("update"), label = "Update")
      ),

      ## UI rendering ----------------------------------------------------------

      # Main content area with div-based layout
      shiny$div(
        # CSS for pill-shaped toggle buttons
        shiny$tags$style(shiny$HTML(
          "
          .toggle-row { display: flex; gap: 10px; margin-bottom: 12px; }
          .toggle-btn {
            border-radius: 999px;
            padding: 6px 18px;
            border: none;
            background: #e0e0e0;
            cursor: pointer;
          }
          .toggle-btn:hover { background: #d0d0d0; }
          .toggle-btn.active {
            background: #007bc2;
            color: white;
          }
        "
        )),

        # Toggle buttons for section visibility (pill-shaped)
        {
          toggle_buttons <- list()
          if (panel_config$features$value_boxes$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_value_boxes"),
                  "Summary",
                  class = "toggle-btn"
                )
              )
            )
          }
          if (panel_config$features$intervention_map$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_intervention_map"),
                  "Intervention Map",
                  class = "toggle-btn"
                )
              )
            )
          }
          if (panel_config$features$impact_analysis$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_impact_analysis"),
                  "Impact Analysis",
                  class = "toggle-btn"
                )
              )
            )
          }
          if (panel_config$features$costs$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_costs"),
                  "Costs",
                  class = "toggle-btn"
                )
              )
            )
          }
          if (panel_config$features$intervention_prioritization$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_intervention_prioritization"),
                  "Prioritization",
                  class = "toggle-btn"
                )
              )
            )
          }
          if (panel_config$features$risk_strata$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_risk_strata"),
                  "Risk Strata",
                  class = "toggle-btn"
                )
              )
            )
          }
          if (panel_config$features$economic_evaluation$enabled) {
            toggle_buttons <- c(
              toggle_buttons,
              list(
                shiny$actionButton(
                  ns("show_economic_evaluation"),
                  "Budget Optimization",
                  class = "toggle-btn"
                )
              )
            )
          }
          shiny$div(class = "toggle-row", !!!toggle_buttons)
        },

        # Disclaimer when no toggles selected
        shiny$uiOutput(ns("no_selection_disclaimer")),

        # Value boxes section
        shiny$uiOutput(ns("value_boxes_section")),

        # Intervention map section
        shiny$uiOutput(ns("intervention_map_section")),

        # Impact Analysis section
        shiny$uiOutput(ns("impact_analysis_section")),

        # Cost & Cost-Effectiveness section
        shiny$uiOutput(ns("costs_section")),

        # Risk Strata Analysis section
        shiny$uiOutput(ns("risk_strata_section")),

        # Intervention Prioritization section
        shiny$uiOutput(ns("intervention_prioritization_section")),

        # Economic Evaluation section — rendered once and toggled via CSS
        # to preserve input state (reference plan, WTP, budget slider).
        if (panel_config$features$economic_evaluation$enabled) {
          shinyjs$hidden(
            shiny$div(
              id = ns("economic_evaluation_section"),
              style = "margin-top: 3px;",
              economic_evaluation$ui(
                id = ns("economic_evaluation"),
                use_custom_data = panel_config$use_custom_data
              )
            )
          )
        }
      )
    )
  )
}


# Server -----------------------------------------------------------------------

#' @export
server <- function(id, variables, country_map, panel_config, ...) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- shiny$NS(id)
    args <- list(...)
    log_ns <- log_ns_builder(args, panel_config$panel_name)

    # Create initialization trigger
    #
    # Simply listening to the input$update button events is not enough. We need
    # to
    # a) trigger the initial plots as well
    # b) ensure that the required variables are set to correct values
    init_count <- shiny$reactiveVal(0)
    init_done <- shiny$reactiveVal(FALSE)

    shiny$observe({
      # Ensure that user input is set
      shiny$req(
        variables$session_state$age_group,
        variables$session_state$agg_level,
        variables$session_state$reference,
        variables$session_state$unit_costs,
        input$age_group
      )

      if (!panel_config$features$scenario_customizer$enabled) {
        shiny$req(
          input$reference
        )
      }

      # Ensure that inputs are not set to defaults/placeholders
      shiny$req(
        input$age_group != character(0),
      )
      if (!panel_config$features$scenario_customizer$enabled) {
        shiny$req(
          input$reference != character(0)
        )
      }

      # NOTE 2025-11-12: I think these checks should never evaluate to TRUE
      #   because agg_level initializes with "National". However, we leave it in
      #   as a Good Practice.
      if (variables$session_state$agg_level == "Regional") {
        shiny$req(variables$session_state$region_selected)
        shiny$req(length(input$region_selected) != 0)
      }
      if (variables$session_state$agg_level == "Risk Strata") {
        shiny$req(variables$session_state$strata_selected)
        shiny$req(length(input$strata_selected) != 0)
      }

      if (!shiny$isolate(init_done())) {
        log_debug("All requirements are met", namespace = log_ns_fn(log_ns))
        shiny$isolate(
          init_count(init_count() + 1)
        )
        init_done(TRUE)
      } else {
        log_debug(
          "Initialization already done, skipping",
          namespace = log_ns_fn(log_ns)
        )
      }
    })

    # Combine button and init triggers
    trigger <- shiny$reactive({
      input$update + init_count()
    })

    ## Toggle button state tracking --------------------------------------------
    ##TODO True for value_boxes but then to turn blue the toggle appearence
    # and do the disclaymer even if valu box is true
    toggle_value_boxes <- shiny$reactiveVal(FALSE)
    toggle_intervention_map <- shiny$reactiveVal(FALSE)
    toggle_impact_analysis <- shiny$reactiveVal(FALSE)
    toggle_costs <- shiny$reactiveVal(FALSE)
    toggle_intervention_prioritization <- shiny$reactiveVal(FALSE)
    toggle_risk_strata <- shiny$reactiveVal(FALSE)
    toggle_economic_evaluation <- shiny$reactiveVal(FALSE)

    # Economic evaluation is supposed to replace the whole page, thus we store
    # the state of toggles before
    saved_toggle_states <- shiny$reactiveVal(list(
      value_boxes = FALSE,
      intervention_map = FALSE,
      impact_analysis = FALSE,
      costs = FALSE,
      risk_strata = FALSE
    ))

    # Toggle observers with CSS class update
    shiny$observe({
      new_val <- !toggle_value_boxes()
      toggle_value_boxes(new_val)
      shinyjs$toggleClass("show_value_boxes", "active", condition = new_val)
      if (new_val && toggle_economic_evaluation()) {
        toggle_economic_evaluation(FALSE)
        shinyjs$removeClass("show_economic_evaluation", "active")
      }
    }) |>
      shiny$bindEvent(input$show_value_boxes)

    shiny$observe({
      new_val <- !toggle_intervention_map()
      toggle_intervention_map(new_val)
      shinyjs$toggleClass(
        "show_intervention_map",
        "active",
        condition = new_val
      )
      if (new_val && toggle_economic_evaluation()) {
        toggle_economic_evaluation(FALSE)
        shinyjs$removeClass("show_economic_evaluation", "active")
      }
    }) |>
      shiny$bindEvent(input$show_intervention_map)

    shiny$observe({
      new_val <- !toggle_impact_analysis()
      toggle_impact_analysis(new_val)
      shinyjs$toggleClass("show_impact_analysis", "active", condition = new_val)
      if (new_val && toggle_economic_evaluation()) {
        toggle_economic_evaluation(FALSE)
        shinyjs$removeClass("show_economic_evaluation", "active")
      }
    }) |>
      shiny$bindEvent(input$show_impact_analysis)

    shiny$observe({
      new_val <- !toggle_costs()
      toggle_costs(new_val)
      shinyjs$toggleClass("show_costs", "active", condition = new_val)
      if (new_val && toggle_economic_evaluation()) {
        toggle_economic_evaluation(FALSE)
        shinyjs$removeClass("show_economic_evaluation", "active")
      }
    }) |>
      shiny$bindEvent(input$show_costs)

    shiny$observe({
      new_val <- !toggle_risk_strata()
      toggle_risk_strata(new_val)
      shinyjs$toggleClass("show_risk_strata", "active", condition = new_val)
      if (new_val && toggle_economic_evaluation()) {
        toggle_economic_evaluation(FALSE)
        shinyjs$removeClass("show_economic_evaluation", "active")
      }
    }) |>
      shiny$bindEvent(input$show_risk_strata)

    shiny$observeEvent(input$show_intervention_prioritization, {
      new_val <- !toggle_intervention_prioritization()
      toggle_intervention_prioritization(new_val)
      shinyjs$toggleClass(
        "show_intervention_prioritization",
        "active",
        condition = new_val
      )
    })
    
    shiny$observe({
      new_val <- !toggle_economic_evaluation()
      toggle_economic_evaluation(new_val)
      shinyjs$toggleClass(
        "show_economic_evaluation",
        "active",
        condition = new_val
      )
      if (new_val) {
        # Save state of all toggles before deactivating
        saved_toggle_states(list(
          value_boxes = toggle_value_boxes(),
          intervention_map = toggle_intervention_map(),
          impact_analysis = toggle_impact_analysis(),
          costs = toggle_costs(),
          risk_strata = toggle_risk_strata()
        ))
        #TODO the same for interventoin_prioritization
        # Deactivate all other toggles
        toggle_value_boxes(FALSE)
        toggle_intervention_map(FALSE)
        toggle_impact_analysis(FALSE)
        toggle_costs(FALSE)
        toggle_risk_strata(FALSE)
        shinyjs$removeClass("show_value_boxes", "active")
        shinyjs$removeClass("show_intervention_map", "active")
        shinyjs$removeClass("show_impact_analysis", "active")
        shinyjs$removeClass("show_costs", "active")
        shinyjs$removeClass("show_risk_strata", "active")
      } else {
        # Restore saved toggle states
        states <- saved_toggle_states()
        toggle_value_boxes(states$value_boxes)
        toggle_intervention_map(states$intervention_map)
        toggle_impact_analysis(states$impact_analysis)
        toggle_costs(states$costs)
        toggle_risk_strata(states$risk_strata)

        # Update CSS classes for restored toggles
        shinyjs$toggleClass(
          "show_value_boxes",
          "active",
          condition = states$value_boxes
        )
        shinyjs$toggleClass(
          "show_intervention_map",
          "active",
          condition = states$intervention_map
        )
        shinyjs$toggleClass(
          "show_impact_analysis",
          "active",
          condition = states$impact_analysis
        )
        shinyjs$toggleClass("show_costs", "active", condition = states$costs)
        shinyjs$toggleClass(
          "show_risk_strata",
          "active",
          condition = states$risk_strata
        )
      }
    }) |>
      shiny$bindEvent(input$show_economic_evaluation)

    # Disclaimer when no toggles are selected
    output$no_selection_disclaimer <- shiny$renderUI({
      any_selected <- toggle_value_boxes() ||
        toggle_intervention_map() ||
        toggle_impact_analysis() ||
        toggle_costs() ||
        toggle_intervention_prioritization() ||
        toggle_risk_strata() ||
        toggle_economic_evaluation()

      if (!any_selected) {
        bslib$card(
          class = "bg-light",
          bslib$card_body(
            shiny$div(
              style = "text-align: center; padding: 2rem; color: #6c757d;",
              shiny$tags$i(
                class = "bi bi-info-circle",
                style = "font-size: 2rem; margin-bottom: 1rem; display: block;"
              ),
              shiny$h5("No display selected"),
              shiny$p(
                "Please select one or more options above to display the corresponding analysis."
              )
            )
          )
        )
      }
    })

    # Conditional rendering for sections
    output$value_boxes_section <- shiny$renderUI({
      if (toggle_value_boxes()) {
        ns <- session$ns
        shiny$div(
          style = "margin-bottom: 10px;",
          value_boxes$ui(id = ns("value_boxes"))
        )
      }
    })

    output$intervention_map_section <- shiny$renderUI({
      if (toggle_intervention_map()) {
        ns <- session$ns
        shiny$div(
          style = "margin-top: 3px;",
          shiny$uiOutput(ns("intervention_maps_ui"))
        )
      }
    })

    output$impact_analysis_section <- shiny$renderUI({
      if (toggle_impact_analysis()) {
        ns <- session$ns
        shiny$div(
          style = "margin-top: 3px;",
          bslib$navset_card_pill(
            id = ns("impact_tabs"),
            title = "Impact Analysis",
            bslib$nav_panel(
              "Figures",
              shiny$uiOutput(ns("impact_figures_ui"))
            ),
            bslib$nav_panel(
              "Tables",
              shiny$uiOutput(ns("impact_tables_ui"))
            ),
            bslib$nav_panel(
              "Maps",
              shiny$uiOutput(ns("impact_maps_content_ui"))
            )
          )
        )
      }
    })

    output$costs_section <- shiny$renderUI({
      if (toggle_costs()) {
        ns <- session$ns
        shiny$div(
          style = "margin-top: 3px;",
          bslib$navset_card_pill(
            id = ns("cost_tabs"),
            title = "Cost & Cost-Effectiveness",
            bslib$nav_panel(
              "Figures",
              shiny$uiOutput(ns("costs_figures_ui"))
            ),
            bslib$nav_panel(
              "Tables",
              shiny$uiOutput(ns("costs_tables_ui"))
            ),
            bslib$nav_panel(
              "Maps",
              shiny$uiOutput(ns("costs_maps_ui"))
            )
          )
        )
      }
    })

    output$risk_strata_section <- shiny$renderUI({
      if (toggle_risk_strata()) {
        ns <- session$ns
        shiny$tagList(
          # CSS for summary value boxes
          shiny$tags$style(shiny$HTML(
            "
            .summary-valuebox {
              background: #f8f9fa;
              border-left: 4px solid #2c7bb6;
              border-radius: 6px;
              padding: 12px 16px;
              margin-bottom: 8px;
              text-align: center;
            }
            .summary-valuebox .vb-value {
              font-size: 2rem;
              font-weight: 700;
              color: #2c3e50;
              line-height: 1.1;
            }
            .summary-valuebox .vb-label {
              font-size: 0.78rem;
              color: #6c757d;
              text-transform: uppercase;
              letter-spacing: 0.04em;
            }
            .summary-valuebox.vb-danger  { border-left-color: #d32f2f; }
            .summary-valuebox.vb-warning { border-left-color: #ffb300; }
            .summary-valuebox.vb-success { border-left-color: #26a69a; }
            .summary-valuebox.vb-info    { border-left-color: #1976d2; }
            "
          )),
          shiny$div(
            style = "margin-top: 3px;",
            strata_analysis$ui(id = ns("strata_analysis"))
          )
        )
      }
    })

    output$intervention_prioritization_section <- shiny$renderUI({
      if (toggle_intervention_prioritization()) {
        ns <- session$ns
        shiny$div(
          style = "margin-top: 3px;",
          intervention_prioritization$ui(id = ns("intervention_prioritization"))
        )
      }
    })

    shiny$observe({
      shinyjs$toggle(
        "economic_evaluation_section",
        condition = toggle_economic_evaluation()
      )
    })

    ## User input --------------------------------------------------------------

    if (!panel_config$features$scenario_customizer$enabled) {
      scenario_picker_simple$server(
        id = "scenario_picker_simple",
        variables = variables
      )
    }

    shinyWidgets$updatePickerInput(
      session = session,
      inputId = "region_selected",
      choices = unique(variables$admins$admin_1),
      selected = unique(variables$admins$admin_1)[1],
    )

    shinyWidgets$updatePickerInput(
      session = session,
      inputId = "strata_selected",
      choices = sort(unique(variables$risk_strata)),
      selected = sort(unique(variables$risk_strata))[1],
    )

    shiny$updateSelectInput(
      session = session,
      inputId = "age_group",
      choices = unique(variables$age_groups)
    )

    shiny$observe({
      variables$set_age_group(input$age_group)
    }) |>
      shiny$bindEvent(input$age_group)

    shiny$observe({
      variables$set_agg_level(input$agg_level)
    }) |>
      shiny$bindEvent(input$agg_level)

    shiny$observe({
      variables$set_region_selected(input$region_selected)
    }) |>
      shiny$bindEvent(input$region_selected)

    shiny$observe({
      variables$set_strata_selected(input$strata_selected)
    }) |>
      shiny$bindEvent(input$strata_selected)

    shiny$observe({
      shiny$req(input$scenario_selected)
      variables$set_scenario_selected(input$scenario_selected)
    }) |>
      shiny$bindEvent(input$scenario_selected)

    # REVIEW: Can't we move this into the scenario_customization module?
    if (panel_config$features$scenario_customizer$enabled) {
      # Sync sidebar scenario selector with scenario_customization module
      shiny$observe({
        shiny$req(input$scenario_selected)
        shiny$updateSelectInput(
          session = session,
          inputId = "scenario_customization-selected_base_plan",
          selected = input$scenario_selected
        )
      }) |>
        shiny$bindEvent(input$scenario_selected)

      # Sync sidebar reference selector with scenario_customization module
      shiny$observe({
        shiny$req(input$reference)
        shiny$updateSelectInput(
          session = session,
          inputId = "scenario_customization-selected_reference",
          selected = input$reference
        )
        # Also update variables directly for data filtering
        variables$set_reference(input$reference)
      }) |>
        shiny$bindEvent(input$reference)
    }

    # Initialize year end slider with database values (start year is locked)
    shiny$observe({
      shiny$req(variables$year_range)
      min_year <- min(variables$year_range)
      max_year <- max(variables$year_range)
      shiny$updateSliderInput(
        session = session,
        inputId = "year_end",
        min = min_year,
        max = max_year,
        value = max_year
      )
      # Initialize session state (start year is always min_year)
      variables$set_year_range(min_year, max_year)
    })

    shiny$observe({
      shiny$req(input$year_end, variables$year_range)
      min_year <- min(variables$year_range)
      variables$set_year_range(min_year, input$year_end)
    }) |>
      shiny$bindEvent(input$year_end)

    # Reactives for year_start and year_end to pass to modules
    year_start_reactive <- shiny$reactive({
      shiny$req(variables$session_state$year_start)
      variables$session_state$year_start
    })

    year_end_reactive <- shiny$reactive({
      shiny$req(variables$session_state$year_end)
      variables$session_state$year_end
    })

    shiny$observe({
      log_debug(
        "Button pressed, triggering update",
        namespace = log_ns_fn(log_ns)
      )
    }) |>
      shiny$bindEvent(input$update)

    shiny$observe({
      cost_names <- names(config$get("cost_defaults"))
      unit_costs <- lapply(cost_names, function(name) {
        input[[paste0("cost_", tolower(name))]]
      })
      names(unit_costs) <- cost_names
      variables$set_unit_costs(unit_costs)
    }) |>
      shiny$bindEvent(
        lapply(names(config$get("cost_defaults")), function(name) {
          input[[paste0("cost_", tolower(name))]]
        })
      )

    shiny$observe({
      lapply(names(config$get("cost_defaults")), function(name) {
        shiny$updateNumericInput(
          session,
          paste0("cost_", tolower(name)),
          value = config$get("cost_defaults")[[name]]
        )
      })
    }) |>
      shiny$bindEvent(input$reset_costs)

    ## Value boxes module ------------------------------------------------------

    if (panel_config$features$value_boxes$enabled) {
      value_boxes$server(
        id = "value_boxes",
        variables = variables,
        use_custom_data = panel_config$use_custom_data,
        year_start = year_start_reactive,
        year_end = year_end_reactive,
        trigger = trigger
      )
    }

    ## Custom data -------------------------------------------------------------

    if (panel_config$features$scenario_customizer$enabled) {
      scenario_customization$server(
        id = "scenario_customization",
        variables = variables,
        trigger = trigger,
        log_ns = log_ns
      )

      # Reverse sync: scenario_customization → sidebar selectors
      shiny$observe({
        selected_base <- input[["scenario_customization-selected_base_plan"]]
        shiny$req(selected_base)
        # Only update if different to avoid loops
        if (!identical(input$scenario_selected, selected_base)) {
          shiny$updateSelectInput(
            session = session,
            inputId = "scenario_selected",
            selected = selected_base
          )
        }
      }) |>
        shiny$bindEvent(input[["scenario_customization-selected_base_plan"]])

      shiny$observe({
        selected_cf <- input[["scenario_customization-selected_reference"]]
        shiny$req(selected_cf)
        # Only update if different to avoid loops
        if (!identical(input$reference, selected_cf)) {
          shiny$updateSelectInput(
            session = session,
            inputId = "reference",
            selected = selected_cf
          )
        }
      }) |>
        shiny$bindEvent(input[["scenario_customization-selected_reference"]])
    }

    ## Dynamic layout ----------------------------------------------------------

    ### Impact Analysis - Figures tab ------------------------------------------

    output$impact_figures_ui <- shiny$renderUI({
      ns <- session$ns

      # 2 plots per row layout
      bslib$layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        # Prevalence/Incidence card
        bslib$navset_card_tab(
          title = "Yearly prevalence and incidence, and rel. reduction",
          full_screen = TRUE,
          # Nav panel for displaying prevalence data
          bslib$nav_panel(
            "Prevalence",
            bslib$card_body(
              yearly_metric$ui(id = ns("prevalence_line_plot"))
            )
          ),
          # Nav panel for displaying incidence data
          bslib$nav_panel(
            "Incidence",
            bslib$card_body(
              yearly_metric$ui(id = ns("incidence_line_plot"))
            )
          ),
          # Nav panel for displaying prevalence reduction data
          bslib$nav_panel(
            "Prevalence reduction",
            bslib$card_body(
              yearly_metric_reduction$ui(
                id = ns("prevalence_reduction_plot")
              )
            )
          ),
          # Nav panel for displaying incidence reduction data
          bslib$nav_panel(
            "Incidence reduction",
            bslib$card_body(
              yearly_metric_reduction$ui(
                id = ns("incidence_reduction_plot")
              )
            )
          )
        ),
        # Relative impact card
        shiny$uiOutput(ns("relative_impact_ui"))
      )
    })

    ### Impact Analysis - Tables tab -------------------------------------------

    output$impact_tables_ui <- shiny$renderUI({
      ns <- session$ns

      bslib$card(
        bslib$card_header("Impact Data Tables"),
        bslib$card_body(
          relative_impact_table$ui(ns("impact_table"))
        )
      )
    })

    ### Impact Analysis - Maps tab ---------------------------------------------

    output$impact_maps_content_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$uiOutput(ns("impact_maps"))
    })

    ### Cost & Cost-Effectiveness - Figures tab --------------------------------

    output$costs_figures_ui <- shiny$renderUI({
      ns <- session$ns

      costs$ui(id = ns("costs"))
    })

    ### Cost & Cost-Effectiveness - Tables tab ---------------------------------

    output$costs_tables_ui <- shiny$renderUI({
      ns <- session$ns

      bslib$card(
        bslib$card_header("Cost Data Tables"),
        bslib$card_body(
          shiny$tableOutput(ns("costs_standalone_table"))
        )
      )
    })

    output$costs_standalone_table <- shiny$renderTable(
      {
        # fmt: skip
        box::use(
          app/logic/interventions/colnames_to_interventions[
            colnames_to_interventions
          ],
          app/logic/analytics/costs[calculate_intervention_costs],
          app/logic/visualization/tables[format_costs_table],
        )

        trigger()

        if (panel_config$use_custom_data) {
          shiny$req(
            variables$session_state$custom_data,
            ncol(variables$session_state$custom_data) > 1
          )
          variables$custom_data_trigger()
          dt <- variables$session_state$custom_data[
            scenario_name %chin% c(variables$session_state$reference, "custom")
          ]
          scenarios_for_table <- c("custom")
        } else {
          # NO year filter - cost calculation needs all years in range.
          # Year filtering is done in calculate_intervention_costs.
          filters <- list(
            age_group = variables$session_state$age_group,
            plan = unname(unlist(lapply(config$get("plans"), names))),
            scenario_name = c(
              variables$scenarios,
              variables$session_state$reference
            )
          )
          if (
            variables$session_state$agg_level == "Regional" &&
              length(variables$session_state$region_selected) > 0
          ) {
            filters <- c(
              filters,
              list(admin_1 = variables$session_state$region_selected)
            )
          }
          if (
            variables$session_state$agg_level == "Risk Strata" &&
              length(variables$session_state$strata_selected) > 0
          ) {
            filters <- c(
              filters,
              list(risk_stratum = variables$session_state$strata_selected)
            )
          }

          dt <- variables$data_con$get_filtered_data(
            filters = filters,
            conversion_rules = variables$conversion_rules,
            log_ns = log_ns
          )
          scenarios_for_table <- variables$scenarios
        }

        dt_int_costs <- calculate_intervention_costs(
          dt = dt,
          interventions = colnames_to_interventions(dt),
          intervention_cols = grep(
            "^deployed_int_.*",
            colnames(dt),
            value = TRUE
          ),
          intervention_cov = grep(
            "^coverage_int_.*",
            colnames(dt),
            value = TRUE
          ),
          unit_costs = variables$session_state$unit_costs,
          year_start = variables$session_state$year_start,
          year_end = variables$session_state$year_end
        )

        format_costs_table(
          dt_int_costs,
          dt,
          shiny$isolate(variables$session_state$reference),
          scenarios_for_table,
          scenario_mapping = variables$scenario_mapping
        )
      },
      bordered = TRUE,
      hover = TRUE,
      spacing = "l",
      sanitize.text.function = identity,
      align = "c"
    )

    ### Cost & Cost-Effectiveness - Maps tab -----------------------------------

    output$costs_maps_ui <- shiny$renderUI({
      ns <- session$ns

      bslib$card(
        bslib$card_header("Cost Maps"),
        bslib$card_body(
          shiny$p("Cost-effectiveness maps will be displayed here.")
        )
      )
    })

    ## Prevalence/Incidence plots ----------------------------------------------

    if (panel_config$features$impact_analysis$enabled) {
      yearly_metric$server(
        id = "prevalence_line_plot",
        variables = variables,
        use_custom_data = panel_config$use_custom_data,
        metric = "prevalenceRate",
        opts = list(y_label = "Prevalence Rate"),
        trigger = trigger,
        log_ns = log_ns
      )

      yearly_metric$server(
        id = "incidence_line_plot",
        variables = variables,
        use_custom_data = panel_config$use_custom_data,
        metric = "incidenceRate",
        opts = list(y_label = "Incidence Rate"),
        trigger = trigger,
        log_ns = log_ns
      )

      yearly_metric_reduction$server(
        id = "prevalence_reduction_plot",
        variables = variables,
        use_custom_data = panel_config$use_custom_data,
        metric = "prevalenceRate",
        opts = list(title = "Prevalence Reduction by Year and Scenario"),
        trigger = trigger,
        log_ns = log_ns
      )

      yearly_metric_reduction$server(
        id = "incidence_reduction_plot",
        variables = variables,
        use_custom_data = panel_config$use_custom_data,
        metric = "incidenceRate",
        opts = list(title = "Incidence Reduction by Year and Scenario"),
        trigger = trigger,
        log_ns = log_ns
      )
    }

    ## Intervention Maps -------------------------------------------------------

    # Create UI dynamically
    output$intervention_maps_ui <- shiny$renderUI({
      # NOTE 2025-06-21: Targeting the right namespace is crucial here! See
      #   https://shiny.posit.co/r/articles/improve/modules/ - "Using renderUI
      #   within modules".
      ns <- session$ns
      scens <- panel_config$get_scenarios(variables)

      # CSS styles for individual intervention maps
      css_styles <- shiny$tags$style(shiny$HTML(
        "
        .small-multiple { padding: 0; }
        .map-title {
          font-size: 0.85rem;
          font-weight: 600;
          text-align: center;
          margin-bottom: 0.25rem;
        }
        .static-maps-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
          gap: 0.5rem;
        }
      "
      ))

      shiny$tagList(
        css_styles,
        bslib$navset_card_tab(
          # Main title for the card
          title = "Intervention Scenarios Map",
          # Enable full screen mode for better map viewing
          full_screen = TRUE,
          # Iterate over scens and generate the UI elements for each
          !!!lapply(
            seq_along(scens),
            function(i) {
              tab_title <- variables$scenario_mapping[scens[i]]
              scenario <- scens[i]

              bslib$nav_panel(
                tab_title,
                # Two-column layout: combined map (1/2) + static maps grid (1/2)
                bslib$layout_column_wrap(
                  width = 1 / 2,
                  # Left column: Combined interventions leaflet map
                  bslib$card(
                    bslib$card_header("All Interventions Combined"),
                    intervention_map$ui(
                      id = ns(paste0(scenario, "_intervention_map"))
                    )
                  ),
                  # Right column: Static maps grid for individual interventions
                  bslib$card(
                    bslib$card_header("Individual Interventions"),
                    shiny$div(
                      class = "static-maps-grid",
                      !!!lapply(variables$intervention_cols, function(int_col) {
                        int_name <- gsub("^deployed_int_", "", int_col)
                        shiny$div(
                          class = "small-multiple",
                          shiny$div(class = "map-title", int_name),
                          static_intervention_map$ui(
                            id = ns(paste0(scenario, "_", int_col, "_static"))
                          )
                        )
                      })
                    )
                  )
                )
              )
            }
          ),
          # The reference tab is generated separately
          bslib$nav_panel(
            "Reference",
            # Two-column layout for reference too
            bslib$layout_column_wrap(
              width = 1 / 2,
              # Left column: Combined interventions leaflet map
              bslib$card(
                bslib$card_header("All Interventions Combined"),
                intervention_map$ui(id = ns("reference_intervention_map"))
              ),
              # Right column: Static maps grid for individual interventions
              bslib$card(
                bslib$card_header("Individual Interventions"),
                shiny$div(
                  class = "static-maps-grid",
                  !!!lapply(variables$intervention_cols, function(int_col) {
                    int_name <- gsub("^deployed_int_", "", int_col)
                    shiny$div(
                      class = "small-multiple",
                      shiny$div(class = "map-title", int_name),
                      static_intervention_map$ui(
                        id = ns(paste0("reference_", int_col, "_static"))
                      )
                    )
                  })
                )
              )
            )
          )
        )
      )
    })

    if (panel_config$features$intervention_map$enabled) {
      # Initialize servers for combined and individual intervention maps
      lapply(seq_along(panel_config$get_scenarios(variables)), function(i) {
        scenario <- panel_config$get_scenarios(variables)[i]

        # Create shared data reactive for this scenario (fetches once)
        scenario_shared_data <- shiny$reactive({
          if (panel_config$use_custom_data) {
            shiny$req(
              variables$session_state$custom_data,
              ncol(variables$session_state$custom_data) > 1
            )
            variables$custom_data_trigger()

            # fmt: skip
            box::use(
              app/logic/data/get_filtered_data[add_intervention_combo_column],
            )

            log_debug(
              "Fetching shared data for custom scenario",
              namespace = log_ns_fn(log_ns)
            )

            dt <- variables$session_state$custom_data[scenario_name == "custom"]
            # Ensure the filtered data is not empty before processing.
            shiny$req(nrow(dt) > 0)
            dt <- add_intervention_combo_column(dt)

            log_debug(
              "Fetched shared data for custom scenario",
              namespace = log_ns_fn(log_ns)
            )

            dt
          } else {
            shiny$req(variables$session_state$reference)
            trigger()

            box::use(
              # fmt: skip
              app/logic/data/get_filtered_data[add_intervention_combo_column],
            )

            log_debug(
              paste0("Fetching shared data for scenario: ", scenario),
              namespace = log_ns_fn(log_ns)
            )

            filters <- list(
              age_group = variables$session_state$age_group,
              plan = unname(unlist(lapply(config$get("plans"), names))),
              scenario_name = scenario,
              year = variables$session_state$year_end
            )
            if (
              variables$session_state$agg_level == "Regional" &&
                length(variables$session_state$region_selected) > 0
            ) {
              filters <- c(
                filters,
                list(admin_1 = variables$session_state$region_selected)
              )
            }
            if (
              variables$session_state$agg_level == "Risk Strata" &&
                length(variables$session_state$strata_selected) > 0
            ) {
              filters <- c(
                filters,
                list(risk_stratum = variables$session_state$strata_selected)
              )
            }

            dt <- variables$data_con$get_filtered_data(
              columns = c(variables$intervention_cols, "admin_1", "admin_2"),
              filters = filters,
              conversion_rules = variables$conversion_rules,
              log_ns = log_ns
            )
            dt <- add_intervention_combo_column(dt)

            log_debug(
              paste0("Fetched shared data for scenario: ", scenario),
              namespace = log_ns_fn(log_ns)
            )

            dt
          }
        })

        # Combined interventions map (uses shared data)
        intervention_map$server(
          id = paste0(scenario, "_intervention_map"),
          country_map_dt = country_map,
          variables = variables,
          use_custom_data = panel_config$use_custom_data,
          scenario_name = scenario,
          year = year_end_reactive,
          trigger = trigger,
          single_intervention = NULL,
          shared_data = scenario_shared_data,
          log_ns = log_ns
        )

        # Create merged SF data reactive for static maps (merge once, reuse)
        scenario_sf_data <- shiny$reactive({
          shiny$req(scenario_shared_data())
          dt <- scenario_shared_data()
          dt <- merge.data.table(dt, country_map, by = c("admin_2", "admin_1"))
          # Convert to sf first, then get unique rows
          sf_data <- st_as_sf(dt)
          sf_data[!duplicated(sf_data$admin_2), ]
        })

        # Individual intervention maps (static ggplot2 maps)
        lapply(variables$intervention_cols, function(int_col) {
          static_intervention_map$server(
            id = paste0(scenario, "_", int_col, "_static"),
            map_data = scenario_sf_data,
            intervention_col = int_col
          )
        })
      })

      # Create shared data reactive for reference (fetches once)
      reference_shared_data <- shiny$reactive({
        if (panel_config$use_custom_data) {
          shiny$req(
            variables$session_state$custom_data,
            ncol(variables$session_state$custom_data) > 1,
            variables$session_state$reference
          )
          variables$custom_data_trigger()

          # fmt: skip
          box::use(
            app/logic/data/get_filtered_data[add_intervention_combo_column],
            )

          log_debug(
            "Fetching shared data for reference (custom)",
            namespace = log_ns_fn(log_ns)
          )

          dt <- variables$session_state$custom_data[
            scenario_name == variables$session_state$reference
          ]
          # Ensure the filtered data is not empty before processing.
          # This handles the race condition when reference is updated
          # before the new cf_data is fetched and combined.
          shiny$req(nrow(dt) > 0)
          dt <- add_intervention_combo_column(dt)

          log_debug(
            "Fetched shared data for reference (custom)",
            namespace = log_ns_fn(log_ns)
          )

          dt
        } else {
          shiny$req(variables$session_state$reference)
          trigger()

          box::use(
            # fmt: skip
            app/logic/data/get_filtered_data[add_intervention_combo_column],
          )

          log_debug(
            "Fetching shared data for reference",
            namespace = log_ns_fn(log_ns)
          )

          filters <- list(
            age_group = variables$session_state$age_group,
            plan = unname(unlist(lapply(config$get("plans"), names))),
            scenario_name = variables$session_state$reference,
            year = variables$session_state$year_end
          )
          if (
            variables$session_state$agg_level == "Regional" &&
              length(variables$session_state$region_selected) > 0
          ) {
            filters <- c(
              filters,
              list(admin_1 = variables$session_state$region_selected)
            )
          }
          if (
            variables$session_state$agg_level == "Risk Strata" &&
              length(variables$session_state$strata_selected) > 0
          ) {
            filters <- c(
              filters,
              list(risk_stratum = variables$session_state$strata_selected)
            )
          }

          dt <- variables$data_con$get_filtered_data(
            columns = c(variables$intervention_cols, "admin_1", "admin_2"),
            filters = filters,
            conversion_rules = variables$conversion_rules,
            log_ns = log_ns
          )
          dt <- add_intervention_combo_column(dt)

          log_debug(
            "Fetched shared data for reference",
            namespace = log_ns_fn(log_ns)
          )

          dt
        }
      })

      # Reference: combined map (uses shared data)
      intervention_map$server(
        id = "reference_intervention_map",
        country_map_dt = country_map,
        variables = variables,
        use_custom_data = panel_config$use_custom_data,
        scenario_name = "current_cf",
        year = year_end_reactive,
        trigger = trigger,
        single_intervention = NULL,
        shared_data = reference_shared_data,
        log_ns = log_ns
      )

      # Create merged SF data reactive for reference static maps
      reference_sf_data <- shiny$reactive({
        shiny$req(reference_shared_data())
        dt <- reference_shared_data()
        dt <- merge.data.table(dt, country_map, by = c("admin_2", "admin_1"))
        # Convert to sf first, then get unique rows
        sf_data <- st_as_sf(dt)
        sf_data[!duplicated(sf_data$admin_2), ]
      })

      # Reference: individual intervention maps (static ggplot2 maps)
      lapply(variables$intervention_cols, function(int_col) {
        static_intervention_map$server(
          id = paste0("reference_", int_col, "_static"),
          map_data = reference_sf_data,
          intervention_col = int_col
        )
      })
    }

    ## Relative impact ---------------------------------------------------------

    # Create UI dynamically
    output$relative_impact_ui <- shiny$renderUI({
      # NOTE 2025-06-21: Targeting the right namespace is crucial here! See
      #   https://shiny.posit.co/r/articles/improve/modules/ - "Using renderUI
      #   within modules".
      ns <- session$ns
      metrics <- names(variables$impact_mapping)

      year_end <- variables$session_state$year_end

      bslib$navset_card_tab(
        # Set the main title for the card
        title = paste0(
          "Relative impact metrics (cum. averted in ",
          year_end,
          ")"
        ),
        # Enable full screen toggle option
        full_screen = TRUE,
        # Use lapply to create multiple tab panels for each metric
        !!!lapply(metrics, function(metric) {
          # Create a tab panel for each metric
          bslib$nav_panel(
            # Use the impact_mapping to get the display name for the metric
            title = variables$impact_mapping[metric],
            # Create the UI component for the metric using the namespaced ID
            relative_impact$ui(
              id = ns(paste0(metric, "_relative_impact"))
            )
          )
        }),
        # Add an additional tab panel for the data table
        bslib$nav_panel(
          title = "Table",
          # Create a table output with namespaced ID
          relative_impact_table$ui(ns("impact_table"))
        )
      )
    })

    # Initialize servers
    if (panel_config$features$impact_analysis$enabled) {
      lapply(seq_along(names(variables$impact_mapping)), function(i) {
        metric <- names(variables$impact_mapping)[i]

        output_name <- paste0(metric, "_relative_impact")

        relative_impact$server(
          id = output_name,
          variables = variables,
          year_start = year_start_reactive,
          year_end = year_end_reactive,
          metric = metric,
          opts = list(),
          trigger = trigger,
          use_custom_data = panel_config$use_custom_data,
          log_ns = log_ns
        )
      })
      # Create the impact table output
      relative_impact_table$server(
        id = "impact_table",
        variables = variables,
        year_start = year_start_reactive,
        year_end = year_end_reactive,
        trigger = trigger,
        use_custom_data = panel_config$use_custom_data,
        log_ns = log_ns,
        scenarios = panel_config$get_scenarios(variables)
      )
    }

    ## Intervention costs ------------------------------------------------------

    if (panel_config$features$costs$enabled) {
      costs$server(
        id = "costs",
        variables = variables,
        year_start = year_start_reactive,
        year_end = year_end_reactive,
        trigger = trigger,
        use_custom_data = panel_config$use_custom_data,
        log_ns = log_ns
      )
    }

    ## Intervention Prioritization ---------------------------------------------

    if (panel_config$features$intervention_prioritization$enabled) {
      intervention_prioritization$server(
        id = "intervention_prioritization",
        variables = variables,
        country_map_dt = country_map,
        use_custom_data = panel_config$use_custom_data,
        trigger = trigger,
        year_start = year_start_reactive,
        year_end = year_end_reactive,
        toggle_enabled = toggle_intervention_prioritization,
        log_ns = log_ns
      )
    }

    ## Risk Strata Analysis ----------------------------------------------------

    if (panel_config$features$risk_strata$enabled) {
      strata_analysis$server(
        id = "strata_analysis",
        variables = variables,
        country_map_dt = country_map,
        use_custom_data = panel_config$use_custom_data,
        trigger = trigger,
        log_ns = log_ns
      )
    }

    ## Economic Evaluation -----------------------------------------------------

    if (panel_config$features$economic_evaluation$enabled) {
      economic_evaluation$server(
        id = "economic_evaluation",
        variables = variables,
        country_map = country_map,
        use_custom_data = panel_config$use_custom_data,
        trigger = trigger,
        is_visible = toggle_economic_evaluation,
        log_ns = log_ns
      )
    }

    ## Impact maps -------------------------------------------------------------

    # Create UI dynamically
    output$impact_maps <- shiny$renderUI({
      # NOTE 2025-06-21: Targeting the right namespace is crucial here! See
      #   https://shiny.posit.co/r/articles/improve/modules/ - "Using renderUI
      #   within modules".
      ns <- session$ns
      metrics <- names(variables$impact_mapping)
      year_end <- variables$session_state$year_end

      bslib$navset_card_tab(
        # Main title for the card
        title = paste0("Impact Metrics (in ", year_end, ")"),
        full_screen = TRUE,
        !!!lapply(metrics, function(metric) {
          bslib$nav_panel(
            title = variables$impact_mapping[metric],
            # 2 maps per row layout
            bslib$layout_column_wrap(
              width = 1 / 2,
              !!!lapply(
                c(
                  panel_config$get_scenarios(variables),
                  "reference"
                ),
                function(n) {
                  bslib$card(
                    bslib$card_header(
                      if (n == "reference") {
                        "Reference"
                      } else {
                        paste0(variables$scenario_mapping[n])
                      }
                    ),
                    impact_map$ui(
                      id = ns(paste0(metric, "_impact_map", "_", n))
                    )
                  )
                }
              )
            )
          )
        })
      )
    })

    # Initialize servers
    lapply(names(variables$impact_mapping), function(metric) {
      lapply(
        c(panel_config$get_scenarios(variables), "reference"),
        function(n) {
          output_name <- paste0(metric, "_impact_map", "_", n)

          impact_map$server(
            id = output_name,
            country_map_dt = country_map,
            variables = variables,
            variable = metric,
            scenario_name = if (n == "reference") {
              shiny$isolate(variables$session_state$reference)
            } else {
              n
            },
            year = year_end_reactive,
            trigger = trigger,
            use_custom_data = panel_config$use_custom_data,
            log_ns = log_ns
          )
        }
      )
    })
  })
}
