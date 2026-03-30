# Dependencies -----------------------------------------------------------------

box::use(
  R6[R6Class],
  data.table[setDT],
  logger[log_debug],
  shiny[reactiveVal, reactiveValues],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/core/varfns,
)

# Code -------------------------------------------------------------------------

#' R6 class holding variable values used throughout a panel
#'
#' @description Manages application variables including:
#' - Database connection details
#' - Data type conversion rules
#' - Scenario definitions and mappings
#' - Administrative divisions and age groups
#' - User selections from UI controls
#'
#' @field data_con Data connection
#' @field conversion_rules List of column type conversion rules for
#'   `convert_col_types`
#' @field all_scenarios Character vector of all scenario names (NSP + BAU)
#' @field scenarios Character vector of NSP scenario names
#' @field references Character vector of BAU scenario names used for
#'   comparison
#' @field intervention_cols Character vector of intervention column names from
#'   database
#' @field admins data.table of unique administrative divisions (admin_1,
#'   admin_2)
#' @field age_groups Character vector of unique age groups
#' @field year_range Numeric vector of available years in ascending order
#' @field admin_mapping Named vector mapping admin levels to display names
#' @field scenario_mapping Named vector mapping scenario IDs to display names
#' @field impact_mapping Named vector mapping metric columns to display names
#' @field session_state ReactiveValues containing:
#'   - age_group: Selected age group
#'   - agg_level: Aggregation level ("admin_1" or "admin_2")
#'   - region_selected: Selected region(s)
#'   - reference: Selected reference scenario
#'   - scenario_selected: Selected main scenario
#'   - custom_data: Data for scenario customization
#' @field custom_data_trigger ReactiveVal that increments when custom data is
#'   ready
#' @export
Variables <- R6Class(
  classname = "Variables",
  public = list(
    # Static variables
    data_con = NULL,
    conversion_rules = NULL,
    all_scenarios = NULL,
    scenarios = NULL,
    references = NULL,
    intervention_cols = NULL,
    admins = NULL,
    risk_strata = NULL,
    age_groups = NULL,
    year_range = NULL,
    admin_mapping = varfns$admin_mapping(),
    scenario_mapping = varfns$scenario_mapping(),
    impact_mapping = varfns$impact_mapping(),

    # Current session state. ReactiveValues holding the last selected values.
    # NOTE 2025-10-21: IMPORTANT This has to be initialized when a new instance
    #   is initialized! Otherwise, this is shared across ALL instances of the
    #   class!
    session_state = NULL,

    # Session Restoration state
    session_restore = NULL,

    # Custom data trigger - incremented when custom data is ready
    custom_data_trigger = NULL,

    # Setters
    # NOTE 2025-05-16: We need to use setter functions, otherwise we encounter
    #   Error in <-: cannot add bindings to a locked environment

    #' @description Set age group
    #' @param age_group Age group as string
    set_age_group = function(age_group) {
      self$session_state$age_group <- age_group
    },

    #' @description Set aggregation level
    #' @param agg_level Aggregation level ("admin_1" or "admin_2")
    set_agg_level = function(agg_level) {
      self$session_state$agg_level <- agg_level
    },

    #' @description Set selected regions
    #' @param region_selected Character vector of region names
    set_region_selected = function(region_selected) {
      self$session_state$region_selected <- region_selected
    },

    #' @description Set selected risk strata
    #' @param strata_selected Character vector of risk strata
    set_strata_selected = function(strata_selected) {
      self$session_state$strata_selected <- strata_selected
    },

    #' @description Set reference scenario
    #' @param reference Display name of reference scenario
    set_reference = function(reference) {
      self$session_state$reference <- names(
        self$scenario_mapping[which(self$scenario_mapping == reference)]
      )
    },

    #' @description Set intervention unit costs
    #' @param costs List of unit costs per intervention
    set_unit_costs = function(costs) {
      self$session_state$unit_costs <- costs
    },

    #' @description Set selected metric
    #' @param value Metric name as string
    set_metric_selected = function(value) {
      self$session_state$metric_selected <- value
    },

    #' @description Set selected scenario
    #' @param scenario Display name of selected scenario
    set_scenario_selected = function(scenario) {
      self$session_state$scenario_selected <- names(
        self$scenario_mapping[which(self$scenario_mapping == scenario)]
      )
    },

    #' @description Set year range (time horizon)
    #' @param year_start Integer start year
    #' @param year_end Integer end year
    set_year_range = function(year_start, year_end) {
      self$session_state$year_start <- as.integer(year_start)
      self$session_state$year_end <- as.integer(year_end)
    },

    #' @description Set custom data
    #' @param data data.table with custom data
    set_custom_data = function(data) {
      self$session_state$custom_data <- data
    },
    #' @description Clear custom data to free RAM
    #' @details Sets custom_data to NULL and triggers garbage collection
    clear_custom_data = function() {
      log_debug("VariablesManager: Clearing custom_data to free RAM")
      self$session_state$custom_data <- NULL
      gc(verbose = FALSE)
    },
    #' @description Set intervention district mapping
    #' @param mapping List mapping interventions to districts.
    #'   This is a list of the form
    #'   list
    #'    - intervention_1
    #'        c("district_A", "district_B")
    #'    - intervention_2
    #'        c("district_C")
    set_intervention_district_mapping = function(mapping) {
      self$session_state$intervention_district_mapping <- mapping
    },
    #' @description Set int_data_pool_ready
    #' @param value Boolean, if init is done.
    set_int_data_pool_ready = function(value) {
      self$session_state$int_data_pool_ready <- value
    },
    #' @description Set state to restore
    #' @param value List of session state.
    set_session_restore_state = function(value) {
      self$session_restore$state <- value
    },
    #' @description Set session_restore$running
    #' @param value Boolean, if restoration is running.
    set_session_restore_running = function(value) {
      self$session_restore$running <- value
    },
    #' @description Set session_restore$stage_completed
    #' @param value Integer, which stage completed last.
    set_session_restore_stage_completed = function(value) {
      self$session_restore$stage_completed <- value
    },
    custom_data_run_trigger = function() {
      # Increment trigger to notify modules that custom data is ready
      self$custom_data_trigger(self$custom_data_trigger() + 1)
    },

    #' @description Initialize Variables instance
    #' @param data_con Data connection
    #' @param conversion_rules List of column type conversion rules
    #' @details Performs asynchronous data loading for:
    #' - Scenarios
    #' - Administrative divisions
    #' - Age groups
    #' - Year range
    #' @note Stops execution if background data loading jobs fail
    initialize = function(data_con, conversion_rules, ...) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "VariablesManager")

      # Execute database queries
      log_debug(
        "Executing initialization queries ...",
        namespace = log_ns_fn(log_ns)
      )

      # Execute queries
      all_scenarios <- data_con$all_scenarios(log_ns = log_ns)
      scenarios <- data_con$scenarios(log_ns = log_ns)
      references <- data_con$references(
        log_ns = log_ns
      )
      intervention_cols <- data_con$intervention_cols(
        log_ns = log_ns
      )
      admins <- data_con$admins(log_ns = log_ns)
      age_groups <- data_con$age_groups(log_ns = log_ns)
      risk_strata <- data_con$risk_strata(log_ns = log_ns)
      year_range <- data_con$year_range(log_ns = log_ns)

      log_debug(
        "All initialization queries completed",
        namespace = log_ns_fn(log_ns)
      )

      self$session_state <- reactiveValues(
        # Selected age group as string
        age_group = NULL,
        # Aggregation level as string
        agg_level = NULL,
        # Selected regions as vector
        region_selected = NULL,
        # Selected risk strata as vector
        strata_selected = NULL,
        # Selected reference as string
        reference = NULL,
        # Selected scenario
        scenario_selected = NULL,
        # Unit costs per intervention as a list
        unit_costs = list(),
        # Selected metric for analysis
        metric_selected = "prevalenceRate",
        # Time horizon year range
        year_start = NULL,
        year_end = NULL,
        # NOTE 2025-11-18: The following values are only relevant for panel 3.
        # Data used for scenario customization
        custom_data = NULL,
        # If intervention data pool has been initialized
        int_data_pool_ready = FALSE,
        # Intervention/districts mapping
        intervention_district_mapping = list(),
      )

      self$session_restore <- reactiveValues(
        # State to restore
        state = NULL,
        # If a session is being restored
        running = FALSE,
        # Last stage completed
        stage_completed = 0
      )

      log_debug(
        "Extracting results ...",
        namespace = log_ns_fn(log_ns)
      )

      self$data_con <- data_con
      self$conversion_rules <- conversion_rules
      self$all_scenarios <- all_scenarios
      self$scenarios <- scenarios
      self$references <- references
      self$intervention_cols <- intervention_cols
      self$admins <- setDT(admins)
      self$risk_strata <- risk_strata
      self$age_groups <- age_groups
      self$year_range <- year_range

      # Initialize custom data trigger
      self$custom_data_trigger <- reactiveVal(0)

      log_debug(
        "Extracting results ... done",
        namespace = log_ns_fn(log_ns)
      )
    }
  )
)
