box::use(
  config,
  data.table[...],
  leaflet[
    colorFactor,
    leafletOutput,
    renderLeaflet
  ],
  logger[log_debug],
  sf[st_as_sf, st_bbox, st_geometry],
  shiny,
)

# fmt: skip
box::use(
  app/logic/visualization/color_palettes[create_intervention_palette],
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/visualization/maps[add_intervention_map],
  app/logic/interventions/sort_interventions[sort_interventions],
)

#' Creates the UI component for the intervention map
#'
#' @param id The module ID used for namespacing between different instances of
#'   the module
#' @return A Shiny UI component containing a Leaflet map output element
#' @export
ui <- function(id) {
  # Create a namespace function for this module
  ns <- shiny$NS(id)

  # Create and return a Leaflet map output element with the namespaced ID
  leafletOutput(ns("intervention_map"), height = "90vh", width = "100%")
}

#' Creates the server logic for the intervention map
#'
#' @param id The module ID used for namespacing between different instances of
#'   the module
#' @param country_map_dt A data.table containing geographic boundary data with
#'   columns:
#'   * admin_1: character - level 1 administrative unit name
#'   * admin_2: character - level 2 administrative unit name
#'   * geometry: sfc_GEOMETRY - geographic boundary data
#' @param variables A list containing user selections and configuration:
#'   * session_state: list with age_group, agg_level, reference, etc.
#'   * intervention_cols: character vector of intervention column names
#'   * db_path: path to database
#'   * conversion_rules: data conversion rules
#' @param scenario_name Character string specifying the scenario to display
#' @param year Numeric value indicating which year's data to display
#' @param trigger Reactive expression to trigger plot updates
#' @param shared_data Optional reactive expression providing pre-fetched data.
#'   If provided, this data will be used instead of fetching from database,
#'   improving performance when multiple maps share the same underlying data.
#' @return A Shiny server function that handles map rendering and updates
#' @export
server <- function(
  id,
  country_map_dt,
  variables,
  scenario_name,
  year,
  trigger,
  use_custom_data = FALSE,
  single_intervention = NULL,
  shared_data = NULL,
  ...
) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "intervention_map")

      # Reactive for data preparation
      map_data <- shiny$reactive({
        # If shared_data is provided, use it instead of fetching shared_data
        # already handles its own trigger dependencies (e.g.,
        # custom_data_trigger)
        if (!is.null(shared_data)) {
          shiny$req(shared_data())
          dt <- shared_data()

          # Select relevant columns based on mode
          if (!is.null(single_intervention)) {
            # For single intervention: keep intervention column
            dt <- dt[,
              c("admin_1", "admin_2", single_intervention),
              with = FALSE
            ]
          } else {
            # For combined view: use intervention_combo
            dt <- dt[, .(admin_1, admin_2, intervention_combo)]
          }
          dt <- unique(dt, by = "admin_2")
          return(dt)
        }

        # Original data fetching logic when shared_data is not provided
        shiny$req(variables$session_state$reference)

        if (use_custom_data) {
          shiny$req(
            variables$session_state$custom_data,
            ncol(variables$session_state$custom_data) > 1
          )
          variables$custom_data_trigger()
        } else {
          trigger()
        }

        # fmt: skip
        box::use(
          app/logic/data/get_filtered_data[add_intervention_combo_column],
          app/logic/interventions/sort_interventions[sort_interventions],
        )

        # Get year value (handle both reactive and static values)
        year_value <- if (shiny$is.reactive(year)) year() else year

        log_debug(
          "Fetching data ...",
          namespace = log_ns_fn(log_ns)
        )
        # Get data
        filters <- list(
          age_group = variables$session_state$age_group,
          plan = unname(unlist(lapply(config$get("plans"), names))),
          scenario_name = if (scenario_name == "current_cf") {
            variables$session_state$reference
          } else {
            scenario_name
          },
          year = year_value
        )
        if (
          variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
            length(variables$session_state$region_selected) > 0
        ) {
          filters <- c(
            filters,
            list(
              admin_1 = variables$session_state$region_selected
            )
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

        dt <- if (use_custom_data) {
          if (scenario_name == "current_cf") {
            variables$session_state$custom_data[
              scenario_name == variables$session_state$reference
            ]
          } else {
            variables$session_state$custom_data[scenario_name == "custom"]
          }
        } else {
          variables$data_con$get_filtered_data(
            columns = c(variables$intervention_cols, "admin_1", "admin_2"),
            filters = filters,
            conversion_rules = variables$conversion_rules,
            log_ns = log_ns
          )
        }
        # Ensure the filtered data is not empty before processing.
        # This handles the race condition when reference is updated
        # before the new cf_data is fetched and combined.
        shiny$req(nrow(dt) > 0)
        log_debug(
          "Fetching data ... done",
          namespace = log_ns_fn(log_ns)
        )

        log_debug(
          "Adding intervention_combo column ...",
          namespace = log_ns_fn(log_ns)
        )
        # Add intervention_combo column if not present
        dt <- add_intervention_combo_column(dt)
        log_debug(
          "Adding intervention_combo column ... done",
          namespace = log_ns_fn(log_ns)
        )

        # Select relevant columns based on mode
        if (!is.null(single_intervention)) {
          # For single intervention: keep intervention column
          dt <- dt[, c("admin_1", "admin_2", single_intervention), with = FALSE]
        } else {
          # For combined view: use intervention_combo
          dt <- dt[, .(admin_1, admin_2, intervention_combo)]
        }
        dt <- unique(dt, by = "admin_2")
        dt
      })

      shiny$observe({
        # Ensure data and map exists before updating
        shiny$req(map_data())
        log_debug(
          "Retrieving intervention map data ...",
          namespace = log_ns_fn(log_ns)
        )
        data <- map_data()
        log_debug(
          "Retrieving intervention map data ... done",
          namespace = log_ns_fn(log_ns)
        )

        # Merge with geographical data
        log_debug(
          "Merging with geospatial data ...",
          namespace = log_ns_fn(log_ns)
        )
        data <- merge.data.table(
          data,
          country_map_dt,
          by = c("admin_2", "admin_1")
        )
        log_debug(
          "Merging with geospatial data ... done",
          namespace = log_ns_fn(log_ns)
        )

        # Calculate map bounds for centering the view
        map_bounds <- as.data.table(
          do.call("rbind", lapply(st_geometry(st_as_sf(data)), st_bbox))
        )
        map_bounds <- map_bounds[,
          .(
            xmin = min(xmin),
            ymin = min(ymin),
            xmax = max(xmax),
            ymax = max(ymax)
          )
        ]

        # Render based on mode
        if (!is.null(single_intervention)) {
          # Single intervention mode: simple binary map
          log_debug(
            paste0("Update single intervention map: ", single_intervention),
            namespace = log_ns_fn(log_ns)
          )
          output$intervention_map <- renderLeaflet({
            ## add_single_intervention_map(
            add_intervention_map(
              data = st_as_sf(data),
              color_pal = colorFactor(
                palette = c("lightgrey", "cornflowerblue"),
                domain = c(FALSE, TRUE)
              ),
              map_bounds = map_bounds,
              intervention_col = single_intervention
            )
          })
        } else {
          # Combined interventions mode: existing logic
          log_debug("Sorting interventions ...", namespace = log_ns_fn(log_ns))
          if (nrow(data) > 0) {
            data <- sort_interventions(
              data,
              interventions = "intervention_combo",
              alphabetical = TRUE,
              sort_components = TRUE
            )
          }
          log_debug(
            "Sorting interventions ... done",
            namespace = log_ns_fn(log_ns)
          )

          # Create color palette for interventions
          pal <- create_intervention_palette(
            data$intervention_combo,
            no_intervention_color = "lightgrey"
          )

          log_debug("Update intervention map", namespace = log_ns_fn(log_ns))
          output$intervention_map <- renderLeaflet({
            add_intervention_map(
              st_as_sf(data),
              pal,
              map_bounds
            )
          })
        }
      })
    }
  )
}
