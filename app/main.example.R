box::use(
  bslib,
  config,
  data.table[setnames],
  logger[
    log_debug,
    log_info,
  ],
  shiny,
  shinyWidgets,
  shinyjs,
  toml,
  tools,
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_fn, setup_logging],
  app/logic/core/panel_config,
  app/logic/core/startup[init_data_connection, load_country_map],
  app/logic/data/variablesManager[Variables],
  ## app/view/panel_2,
  ## app/view/panel_3,
  app/view/panel_about,
  app/view/panel_assumptions,
  app/view/panel_scenario_explorer,
)

# Global setup -----------------------------------------------------------------

shiny::enableBookmarking(store = "url")
.datatable.aware <- TRUE # nolint

setup_logging()

log_info(
  "R_CONFIG_ACTIVE = {Sys.getenv(\"R_CONFIG_ACTIVE\", unset = NA)}",
  namespace = log_ns_fn("main")
)

# Data connection
data_con <- init_data_connection(log_ns = "main")

shiny$onStop(function() {
  # Close DB connection pool
  data_con$shutdown(log_ns = "main")
})

# Country map
country_map <- load_country_map(log_ns = "main")

# Replace admin_1 values with Region_Nam, they are prettier
if ("admin_1" %in% names(country_map)) {
  country_map[, admin_1 := NULL]
}
setnames(country_map, old = "Region_Nam", new = "admin_1")

# Panel Config
panel_2_config <- panel_config$PanelConfig$new(
  panel_name = "panel_2",
  title = "Impact of Core National Scenarios",
  use_custom_data = FALSE,
  get_scenarios = function(variables) variables$scenarios,
  log_ns = "main"
)

panel_3_config <- panel_config$PanelConfig$new(
  panel_name = "panel_3",
  title = "Customization & Optimization",
  use_custom_data = TRUE,
  get_scenarios = function(variables) c("custom"),
  log_ns = "main"
)

# Main UI/Server ---------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    title = shiny$span(
      shiny$img(src = config$get("logo_path")),
      config$get("dashboard_title"),
      bslib$popover(
        title = "Session Management",
        options = list(trigger = "focus"),
        shiny$actionButton(
          ns("state_menu"),
          "Session",
          class = "btn btn-outline-primary btn-sm",
          style = "margin-left: 15px;"
        ),
        # REVIEW 2025-12-08: The restoration via bookmarking needs some more
        #   work, e.g. hook it up with the restoration process from TOML files.
        #   Due to time constraints, this feature will be left out for now as it
        #   is easy enough to share the TOML file.
        ## shiny$bookmarkButton(
        ##   label = "Share link",
        ##   class = "btn btn-outline-primary btn-sm w-100 mb-2"
        ## ),
        shiny$downloadButton(
          ns("save_state"),
          "Save Session",
          class = "btn btn-primary btn-sm w-100 mb-2"
        ),
        shiny$fileInput(
          inputId = ns("upload_state"),
          label = "Restore Session",
          accept = c(".toml", "text/plain")
        )
      )
    ),
    # Set up shinyjs
    shinyjs$useShinyjs(),
    # Busy indicators provide a better UX as the user can see that something is
    # happening while waiting
    shiny$useBusyIndicators(),
    panel_about$ui(ns("panel_about")),
    panel_scenario_explorer$ui(
      ns("panel_2"),
      panel_config = panel_2_config
    ),
    panel_scenario_explorer$ui(
      ns("panel_3"),
      panel_config = panel_3_config
    ),
    # DEPRECATED: Remove after validated
    ## panel_2$ui(ns("panel_2_old")),
    ## panel_3$ui(ns("panel_3_old")),
    panel_assumptions$ui(ns("panel_assumptions"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # Initialize both Variables instances
    log_debug(
      "Initializing Variables instances ...",
      namespace = log_ns_fn("main")
    )
    Variables_panel_2 <- Variables$new(
      data_con,
      config$get("conversion_rules"),
      log_ns = "main:panel_2"
    )
    Variables_panel_3 <- Variables$new(
      data_con,
      config$get("conversion_rules"),
      log_ns = "main:panel_3"
    )
    log_debug(
      "Variables initialization complete",
      namespace = log_ns_fn("main")
    )

    panel_scenario_explorer$server(
      "panel_2",
      panel_config = panel_2_config,
      variables = Variables_panel_2,
      country_map = country_map,
      log_ns = "main"
    )

    panel_scenario_explorer$server(
      "panel_3",
      panel_config = panel_3_config,
      variables = Variables_panel_3,
      country_map = country_map,
      log_ns = "main"
    )

    # DEPRECATED: Remove after validated
    ## panel_2$server(
    ##   id = "panel_2_old",
    ##   variables = Variables_panel_2,
    ##   country_map = country_map,
    ##   log_ns = "main"
    ## )

    # DEPRECATED: Remove after validated
    ## panel_3$server(
    ##   id = "panel_3_old",
    ##   variables = Variables_panel_3,
    ##   country_map = country_map,
    ##   log_ns = "main"
    ## )

    panel_assumptions$server(id = "panel_assumptions")
  })
}
