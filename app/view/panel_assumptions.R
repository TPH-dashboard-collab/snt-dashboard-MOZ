box::use(
  bslib,
  config,
  shiny,
)


# UI ---------------------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$nav_panel(
    "Assumptions & Calibration",
    shiny$htmlOutput(ns("ass"))
  )
}


# Server -----------------------------------------------------------------------

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- shiny$NS(id)

    output$ass <- shiny$renderUI({
      ## tags$iframe(
      ##   src = base64enc::dataURI(file="data/Calib_TZA.html", mime="text/html; charset=UTF-8"),
      ##   style="border:0; position:relative; top:0; left:0; right:0; bottom:0; width:100%; height:1800px"
      ## )
      shiny$tags$iframe(
        seamless = "seamless",
        src = config$get("calibration_html"),
        style = "width: 100%; height: 800px; border: none;"
      )
    })
  })
}
