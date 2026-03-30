box::use(
  shiny,
)

# fmt: skip
box::use(
  app/logic/visualization/maps[render_static_intervention_map],
)

#' UI component for static intervention map
#'
#' Creates a plotOutput for displaying a static ggplot2-based intervention map.
#' Designed for small-multiple display with fixed height.
#'
#' @param id Module ID for namespacing
#' @return A shiny plotOutput element
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$plotOutput(ns("static_map"), height = "150px", width = "100%")
}

#' Server component for static intervention map
#'
#' Renders a static ggplot2 map showing deployment status for a single
#' intervention. Uses shared data to avoid redundant computations.
#'
#' @param id Module ID for namespacing
#' @param map_data Reactive expression returning an sf object with geometry
#'   and intervention columns
#' @param intervention_col Character string specifying which intervention
#'   column to display (e.g., "deployed_int_CM")
#' @export
server <- function(id, map_data, intervention_col) {
  shiny$moduleServer(id, function(input, output, session) {
    output$static_map <- shiny$renderPlot(
      {
        shiny$req(map_data())
        render_static_intervention_map(
          data = map_data(),
          intervention_col = intervention_col
        )
      },
      bg = "transparent"
    )
  })
}
