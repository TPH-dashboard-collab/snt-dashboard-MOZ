box::use(
  bslib,
  config,
  logger[log_debug],
  shiny,
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
)

# UI ---------------------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  # Top section: Scenario & Reference
  bslib$card(
    bslib$card_header("Scenario & Reference"),
    bslib$card_body(
      # Selection of the scenario
      shiny$selectInput(
        inputId = ns("scenario_selected"),
        label = "Choose scenario:",
        choices = character(0)
      ),
      # Selection of the reference scenario
      shiny$selectInput(
        inputId = ns("reference"),
        label = "Choose reference:",
        choices = character(0)
      )
    )
  )
}

# Server -----------------------------------------------------------------------

#' @export
server <- function(id, variables, ...) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- shiny$NS(id)
    args <- list(...)
    log_ns <- log_ns_builder(args, "scenario_picker_simple")

    # Scenario selectors for NSP and reference scenarios
    shiny$updateSelectInput(
      session = session,
      inputId = "scenario_selected",
      choices = unname(unlist(config$get("scenario_mapping"))[unlist(config$get(
        "plans"
      )[["scenarios"]])])
    )

    shiny$updateSelectInput(
      session = session,
      inputId = "reference",
      choices = unname(unlist(config$get("scenario_mapping"))[unlist(config$get(
        "plans"
      )[["references"]])])
    )

    ## User input --------------------------------------------------------------

    shiny$observe({
      variables$set_scenario_selected(input$scenario_selected)
    }) |>
      shiny$bindEvent(input$scenario_selected)

    shiny$observe({
      variables$set_reference(input$reference)
    }) |>
      shiny$bindEvent(input$reference)
  })
}
