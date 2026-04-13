box::use(
  bslib[card, card_body, card_header, layout_columns, nav_panel],
  shiny[NS, div, h3, p, strong, tags],
)

# UI ---------------------------------------------------------------------------

#' @export
ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "About",
    div(
      class = "container-fluid py-4",
      # Hero section
      div(
        class = "mb-4",
        h3("Example Dashboard", class = "mb-3"),
        div(
          class = "alert alert-warning d-inline-block",
          role = "alert",
          strong("Work in progress: "),
          "This dashboard is still under development. Some functions may not",
          "work as expected, and results may change as the tool is refined."
        )
      ),
      # Tab description cards
      layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,
        card(
          card_header(
            class = "bg-primary text-white",
            "Tab 1: Impact of core national scenarios"
          ),
          card_body(
            p(
              "Compares the impact of national reference",
              "scenarios in the NMSP."
            ),
            tags$ul(
              class = "list-group list-group-flush",
              tags$li(
                class = "list-group-item px-0",
                strong("BAU"),
                " (Business As Usual): ",
                "currently funded interventions, including case management and",
                "selected vector control interventions"
              ),
              tags$li(
                class = "list-group-item px-0",
                strong("NSP"),
                " (Ideal / Unfunded): ",
                "an aspirational reference scenario including case management,",
                "vector control, and selected human-targeted interventions",
                "(e.g. IPTsc, vaccines, PMC, SMC)"
              )
            )
          )
        ),
        card(
          card_header(
            class = "bg-primary text-white",
            "Tab 2: Impact of customized scenarios"
          ),
          card_body(
            p(
              "Allows users to build and compare customized sub-national",
              "intervention packages in the NMSP using a",
              "range of malaria interventions:"
            ),
            tags$div(
              class = "row row-cols-2 g-2",
              tags$div(
                class = "col",
                tags$span(class = "badge bg-light text-dark p-2 w-100", "IPTsc")
              ),
              tags$div(
                class = "col",
                tags$span(
                  class = "badge bg-light text-dark p-2 w-100",
                  "ITNs (STD/PBO/IG2)"
                )
              ),
              tags$div(
                class = "col",
                tags$span(class = "badge bg-light text-dark p-2 w-100", "IRS")
              ),
              tags$div(
                class = "col",
                tags$span(
                  class = "badge bg-light text-dark p-2 w-100",
                  "Malaria vaccines"
                )
              ),
              tags$div(
                class = "col",
                tags$span(
                  class = "badge bg-light text-dark p-2 w-100",
                  "CM / iCCM"
                )
              ),
              tags$div(
                class = "col",
                tags$span(class = "badge bg-light text-dark p-2 w-100", "PMC")
              ),
              tags$div(
                class = "col",
                tags$span(class = "badge bg-light text-dark p-2 w-100", "SMC")
              ),
              tags$div(
                class = "col",
                tags$span(class = "badge bg-light text-dark p-2 w-100", "LSM")
              )
            )
          )
        ),
        card(
          card_header(
            class = "bg-primary text-white",
            "Tab 3: Model assumptions"
          ),
          card_body(
            p(
              "Summarizes the key assumptions used in",
              "the model, including transmission",
              "seasonality, vector species contributions,",
              "and historical coverage of major malaria",
              "interventions."
            )
          )
        )
      )
    )
  )
}
