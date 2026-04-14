box::use(
  config,
  data.table[...],
  leaflet[
    colorNumeric,
    leafletOutput,
    renderLeaflet
  ],
  logger[log_debug],
  sf[st_as_sf, st_bbox, st_geometry],
  shiny,
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/visualization/maps[add_impact_map],
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
  leafletOutput(ns("impact_map"))
}

#' Creates the server logic for the impact map
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
#' @return A Shiny server function that handles map rendering and updates
#' @export
server <- function(
  id,
  country_map_dt,
  variables,
  use_custom_data = FALSE,
  scenario_name,
  year,
  variable,
  trigger,
  ...
) {
  shiny$moduleServer(
    id = id,
    module = function(input, output, session) {
      args <- list(...)
      log_ns <- log_ns_builder(args, "impact_map")

      # Reactive for data preparation
      # FIXME 2025-06-27: Here is a potential performance issue: We always fetch
      #   all scenarios from the DB and filter below, because otherwise the
      #   coloring is not correct (we need to take the cases in all scenarios
      #   into account for the color gradient).
      map_data <- shiny$reactive({
        if (use_custom_data) {
          shiny$req(
            variables$session_state$custom_data,
            ncol(variables$session_state$custom_data) > 1
          )
          variables$custom_data_trigger()
        } else {
          trigger()
        }

        shiny$req(variables$session_state$age_group)

        box::use(
          data.table[dcast],
        )

        # fmt: skip
        box::use(
          app/logic/data/data_connection_sqlite[DataConnectionSQLite],
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
          variables$session_state$custom_data
        } else {
          variables$data_con$get_filtered_data(
            filters = filters,
            conversion_rules = variables$conversion_rules,
            log_ns = log_ns
          )
        }
        log_debug(
          "Fetching data ... done",
          namespace = log_ns_fn(log_ns)
        )

        log_debug(
          "Aggregating data ...",
          namespace = log_ns_fn(log_ns)
        )

        # Aggregate data for the specified year
        dt <- dt[
          year == year,
          .(
            cum_nUncomp = mean(cum_nUncomp),
            cum_nSevere = mean(cum_nSevere),
            cum_tUncomp = mean(cum_tUncomp),
            cum_tSevere = mean(cum_tSevere),
            cum_expectedDirectDeaths = mean(cum_expectedDirectDeaths)
          ),
          by = .(scenario_name, admin_1, admin_2, EIR_CI)
        ]

        # Reshape data from long to wide format
        dt <- dcast(
          dt,
          scenario_name + admin_1 + admin_2 ~ EIR_CI,
          value.var = c(
            "cum_nUncomp",
            "cum_nSevere",
            "cum_tUncomp",
            "cum_tSevere",
            "cum_expectedDirectDeaths"
          )
        )
        log_debug(
          "Aggregating data ... done",
          namespace = log_ns_fn(log_ns)
        )

        dt
      })

      shiny$observe({
        # Ensure data and map exists before updating
        shiny$req(map_data())
        log_debug(
          "Retrieving impact map data ...",
          namespace = log_ns_fn(log_ns)
        )
        data <- map_data()
        log_debug(
          "Retrieving impact map data ... done",
          namespace = log_ns_fn(log_ns)
        )

        # Merge with geographic data
        log_debug(
          "Merging with geospatial data ...",
          namespace = log_ns_fn(log_ns)
        )
        data <- merge.data.table(
          data,
          country_map_dt,
          by = c("admin_1", "admin_2")
        )
        log_debug(
          "Merging with geospatial data ... done",
          namespace = log_ns_fn(log_ns)
        )

        # Clean up column names
        setnames(
          data,
          old = 2:length(names(data)),
          new = function(x) {
            gsub("_EIR_", "_", x)
          }
        )

        # Select relevant columns
        cols <- c(
          "scenario_name",
          "geometry",
          "admin_1",
          "admin_2",
          paste0(variable, c("_mean", "_lci", "_uci"))
        )

        # Select relevant columns
        data <- data[, ..cols]

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

        # Define the column to use for coloring
        col <- paste0(variable, "_mean")

        # Create color palette
        pal <- colorNumeric(
          palette = "YlOrRd",
          c(data[[col]])
        )

        # Filter data for the specified scenario
        sc <- if (scenario_name == "current_cf") {
          shiny$isolate(variables$session_state$reference)
        } else {
          scenario_name
        }
        data <- data[scenario_name == sc]

        log_debug("Update impact map", namespace = log_ns_fn(log_ns))
        # Update the Leaflet map
        output$impact_map <- renderLeaflet({
          add_impact_map(
            st_as_sf(data),
            pal,
            map_bounds,
            col
          )
        })
      })
    }
  )
}
