# Dependencies -----------------------------------------------------------------

box::use(
  R6[R6Class],
  config,
  logger[log_debug],
  rlang[`%||%`],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
)

# Code -------------------------------------------------------------------------

#' R6 class for panel configuration
#'
#' @description Stores configuration for a dashboard panel including panel
#'   metadata, feature settings, and scenario retrieval function. Provides
#'   unified access to panel-specific configuration loaded from config files or
#'   provided at initialization.
#'
#' @field panel_name Character string identifying the panel
#' @field title Character string display title for the panel
#' @field use_custom_data Logical flag indicating whether to use custom
#'   (user-provided) data (default: FALSE)
#' @field features List of features enabled for this panel
#' @field get_scenarios Function to retrieve available scenarios for the panel
#'
#' @export
PanelConfig <- R6Class(
  "PanelConfig",
  public = list(
    panel_name = NULL,
    title = NULL,
    use_custom_data = FALSE,
    features = NULL,
    get_scenarios = NULL,

    initialize = function(
      panel_name,
      title,
      use_custom_data = FALSE,
      features = NULL,
      get_scenarios = NULL,
      ...
    ) {
      args <- list(...)
      log_ns <- log_ns_builder(args, paste0("PanelConfig", ":", panel_name))

      log_debug(
        "Initializing config ...",
        namespace = log_ns_fn(log_ns)
      )
      self$panel_name <- panel_name
      self$title <- title
      self$use_custom_data <- use_custom_data
      self$features <- features %||% config$get("features")[[panel_name]]
      self$get_scenarios <- get_scenarios

      log_debug(
        "Initializing config ... done",
        namespace = log_ns_fn(log_ns)
      )
    }
  )
)
