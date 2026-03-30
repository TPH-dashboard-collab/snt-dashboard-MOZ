box::use(
  checkmate[qtest],
  ggplot2[
    aes,
    geom_sf,
    ggplot,
    margin,
    scale_fill_manual,
    theme,
    theme_void
  ],
  htmltools[HTML],
  leaflet[
    addLegend,
    addPolygons,
    addProviderTiles,
    clearControls,
    clearShapes,
    colorFactor,
    fitBounds,
    highlightOptions,
    labelOptions,
    leaflet,
    providers,
  ],
)

#' Add intervention map to Leaflet proxy
#'
#' Updates a Leaflet map proxy with polygons representing intervention data,
#' including styling, labels, legend, and map bounds.
#'
#' @param data Spatial data (sf object) containing intervention data and
#'   geometry. Must include columns:
#'   * intervention_combo: character
#'   * geometry: sfc_GEOMETRY
#'   * admin_2: character
#' @param color_pal Color palette function for mapping intervention combinations
#' @param map_bounds List with bounds coordinates (xmin, xmax, ymin, ymax)
#' @param intervention_col Name of the intervention column
#' @return Leaflet object
#' @export
add_intervention_map <- function(data,
                                 color_pal,
                                 map_bounds,
                                 intervention_col = NULL) {
  # Input validation
  stopifnot(
    "color_pal must be a function" = is.function(color_pal),
    "map_bounds must be a list or data frame" =
      qtest(map_bounds, "L+") || qtest(map_bounds, "D+"),
    "map_bounds must contain xmin, xmax, ymin, ymax" =
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(map_bounds)),
    "data must contain 'admin_2' column" = "admin_2" %in% names(data)
  )

  if (is.null(intervention_col)) {
    stopifnot(
      "data must contain 'intervention_combo' when intervention_col is NULL" =
        "intervention_combo" %in% names(data)
    )
  } else {
    stopifnot(
      "intervention_col must be a string" = qtest(intervention_col, "S1"),
      "intervention_col must be a column in data" =
        intervention_col %in% names(data)
    )
  }

  # Create labels for each geographic area
  labels <- if (is.null(intervention_col)) {
    sprintf(
      "<strong>%s</strong><br/>Interventions: %s",
      data$admin_2, data$intervention_combo
    ) |> lapply(HTML)
  } else {
    sprintf(
      "<strong>%s</strong><br/>Status: %s",
      data$admin_2,
      ifelse(data[[intervention_col]], "Deployed", "Not Deployed")
    ) |> lapply(HTML)
  }

  leaflet(data = data) |>
    addProviderTiles(providers$CartoDB) |>
    # Clear existing shapes and controls
    clearShapes() |>
    clearControls() |>
    # Add polygons for each geographic area
    addPolygons(
      fillColor = ~ if (is.null(intervention_col)) {
        color_pal(intervention_combo)
      } else {
        color_pal(data[[intervention_col]])
      },
      fillOpacity = 0.7,
      color = "black",
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      group = ~admin_2,
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    # Add a legend
    addLegend(
      position = "bottomright",
      colors = ~ if (is.null(intervention_col)) {
        unique(color_pal(intervention_combo))
      } else {
        c("cornflowerblue", "lightgrey")
      },
      labels = ~ if (is.null(intervention_col)) {
        unique(intervention_combo)
      } else {
        c("Deployed", "Not Deployed")
      },
      opacity = 0.7
    ) |>
    # Fit the map view to the data bounds
    fitBounds(
      lng1 = map_bounds$xmax, lat1 = map_bounds$ymax,
      lng2 = map_bounds$xmin, lat2 = map_bounds$ymin
    )
}

#' Add impact map to Leaflet proxy
#'
#' Updates a Leaflet map proxy with polygons representing impact data,
#' including styling, labels, legend, and map bounds.
#'
#' @param data Spatial data (sf object) containing impact data and
#'   geometry. Must include columns:
#'   * impact_combo: character
#'   * geometry: sfc_GEOMETRY
#'   * admin_2: character
#' @param color_pal Color palette function for mapping impact combinations
#' @param map_bounds List with bounds coordinates (xmin, xmax, ymin, ymax)
#' @return Updated Leaflet proxy object
#' @export
add_impact_map <- function(data, color_pal, map_bounds, col) {
  # Input validation
  stopifnot(
    "color_pal must be a function" = is.function(color_pal),
    "map_bounds must be a list or data frame" =
      qtest(map_bounds, "L+") || qtest(map_bounds, "D+"),
    "map_bounds must contain xmin, xmax, ymin, ymax" =
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(map_bounds)),
    "col must be a string" = qtest(col, "S1"),
    "col must be a column in data" = col %in% names(data),
    "data must contain 'admin_2' column" = "admin_2" %in% names(data)
  )

  # Create labels for each geographic area
  labels <- sprintf(
    "<strong>%s</strong><br/>Cases: %s",
    data$admin_2, format(round(data[[col]]), big.mark = " ")
  ) |> lapply(HTML)

  # Update the Leaflet map
  leaflet(data = data) |>
    addProviderTiles(providers$CartoDB) |>
    # Clear existing shapes and controls
    clearShapes() |>
    clearControls() |>
    # Add polygons for each geographic area
    addPolygons(
      fillColor = ~ color_pal(data[[col]]),
      fillOpacity = 0.7,
      color = "black",
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    fitBounds(
      lng1 = map_bounds$xmax, lat1 = map_bounds$ymax,
      lng2 = map_bounds$xmin, lat2 = map_bounds$ymin
    )
}

#' Render a static intervention map using ggplot2
#'
#' Creates a minimal static map showing deployed/not-deployed status for a
#' single intervention. Designed for small-multiple display with tight spacing.
#'
#' @param data Spatial data (sf object) containing intervention data and
#'   geometry. Must include a column matching intervention_col with logical
#'   values (TRUE = deployed, FALSE = not deployed).
#' @param intervention_col Character string specifying the column name
#'   containing intervention deployment status (e.g., "deployed_int_CM").
#' @return A ggplot2 object
#' @export
render_static_intervention_map <- function(data, intervention_col) {
  ggplot(data) +
    geom_sf(
      aes(fill = .data[[intervention_col]]),
      color = "black",
      linewidth = 0.1
    ) +
    scale_fill_manual(
      values = c("FALSE" = "lightgrey", "TRUE" = "#6e94da"),
      guide = "none"
    ) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

#' Add risk strata map to Leaflet
#'
#' Creates a Leaflet map with polygons representing risk strata data,
#' including styling, labels, legend, and map bounds.
#'
#' @param geo_data Spatial data (sf object) containing risk strata data and
#'   geometry. Must include columns:
#'   * admin_2: character (geographic area name)
#'   * Column specified by risk_col: character (risk stratum category)
#'   * geometry: sfc_GEOMETRY
#' @param risk_colors Named vector of colors for risk strata levels
#' @param map_bounds List with bounds coordinates (xmin, xmax, ymin, ymax)
#' @param risk_col Name of the column containing risk stratum values
#'   (default: "risk_stratum")
#' @param labels Optional pre-built HTML labels. If NULL, default labels are
#'   generated.
#' @param legend_title Title for the legend (default: "Risk Stratum")
#' @return Leaflet object
#' @export
add_strata_map <- function(geo_data,
                           risk_colors,
                           map_bounds,
                           risk_col = "risk_stratum",
                           labels = NULL,
                           legend_title = "Risk Stratum") {

  # Create color palette
  color_pal <- colorFactor(
    palette = unname(risk_colors),
    levels = names(risk_colors),
    na.color = "#D3D3D3"
  )

  # Create default labels if not provided
  if (is.null(labels)) {
    labels <- sprintf(
      "<strong>%s</strong><br/>Risk Stratum: %s",
      geo_data$admin_2,
      geo_data[[risk_col]]
    ) |> lapply(HTML)
  }

  leaflet(data = geo_data) |>
    addProviderTiles(providers$CartoDB) |>
    clearShapes() |>
    clearControls() |>
    addPolygons(
      fillColor = color_pal(geo_data[[risk_col]]),
      fillOpacity = 0.8,
      color = "white",
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.8,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    addLegend(
      position = "bottomright",
      pal = color_pal,
      values = names(risk_colors),
      title = legend_title,
      opacity = 0.8
    ) |>
    fitBounds(
      lng1 = map_bounds$xmax, lat1 = map_bounds$ymax,
      lng2 = map_bounds$xmin, lat2 = map_bounds$ymin
    )
}

#' Create HTML labels for risk strata transition map
#'
#' Generates HTML-formatted labels showing risk strata transitions between
#' two years for use in Leaflet map tooltips.
#'
#' @param geo_data Spatial data (sf object) containing transition data with
#'   columns:
#'   * admin_2: character (geographic area name)
#'   * risk_stratum_{yr_start}: character (start year risk stratum)
#'   * risk_stratum_{yr_end}: character (end year risk stratum)
#'   * prevalenceRate_{yr_start}: numeric (start year prevalence)
#'   * prevalenceRate_{yr_end}: numeric (end year prevalence)
#' @param yr_start Start year (integer)
#' @param yr_end End year (integer)
#' @return List of HTML-formatted label elements for Leaflet
#' @export
create_map_labels <- function(geo_data, yr_start, yr_end) {
  stopifnot(
    "yr_start must be an integer" = qtest(yr_start, "X1"),
    "yr_end must be an integer" = qtest(yr_end, "X1"),
    "geo_data must contain 'admin_2' column" = "admin_2" %in% names(geo_data)
  )

  col_risk_start <- paste0("risk_stratum_", yr_start)
  col_risk_end <- paste0("risk_stratum_", yr_end)
  col_prev_start <- paste0("prevalenceRate_", yr_start)
  col_prev_end <- paste0("prevalenceRate_", yr_end)

  required_cols <- c(col_risk_start, col_risk_end, col_prev_start, col_prev_end)
  missing_cols <- setdiff(required_cols, names(geo_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  sprintf(
    "<strong>%s</strong><br/>
    %s: %s (%.1f%% Prev.)<br/>
    %s: %s (%.1f%% Prev.)<br/>
    <em>Movement: %s to %s</em><br/>
    <small style='color: grey;'>*Prevalence Rates</small>",
    geo_data$admin_2,
    yr_start,
    geo_data[[col_risk_start]],
    geo_data[[col_prev_start]] * 100,
    yr_end,
    geo_data[[col_risk_end]],
    geo_data[[col_prev_end]] * 100,
    geo_data[[col_risk_start]],
    geo_data[[col_risk_end]]
  ) |> lapply(HTML)
}
