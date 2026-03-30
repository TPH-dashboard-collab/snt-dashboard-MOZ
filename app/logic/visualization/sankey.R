# fmt: skip
box::use(
  data.table[as.data.table, data.table, setnames],
  dplyr[
    across, all_of, distinct, filter, group_by,
    left_join, mutate, n, rename, select, summarise,
  ],
  plotly[layout, plot_ly],
  shiny[req],
  tidyr[drop_na, pivot_wider],
)

# fmt: skip
box::use(
  app/logic/core/varfns[classify_risk],
  app/logic/visualization/color_palettes[get_risk_colors],
)


#' Prepare wide-format data for Sankey diagram
#'
#' Pivots risk strata data into wide format with year columns for Sankey
#' transitions. Also calculates population weights per district.
#'
#' @param df Data frame with risk strata data containing columns:
#'   * admin_2: character (district name)
#'   * year: integer
#'   * risk_stratum: character/factor
#'   * nHost: numeric (population)
#' @param selected_years Numeric vector of years to include
#' @return Wide data frame with yr_YYYY columns or NULL if no data
#' @export
prep_sankey_data <- function(df, selected_years) {
  req(length(selected_years) >= 2)
  years <- sort(selected_years)

  df_filtered <- df |> filter(year %in% years)
  if (nrow(df_filtered) == 0) {
    return(NULL)
  }

  # Aggregate to handle multiple plans - take mean prevalence per district/year
  df_agg <- df_filtered |>
    group_by(admin_2, year) |>
    summarise(
      prevalenceRate = mean(prevalenceRate, na.rm = TRUE),
      nHost = mean(nHost, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Recalculate risk_stratum based on aggregated prevalence
    mutate(risk_stratum = classify_risk(prevalenceRate))

  # Get population weights (use mean across selected years for stability)
  pop_weights <- df_agg |>
    group_by(admin_2) |>
    summarise(nHost = mean(nHost, na.rm = TRUE), .groups = "drop")

  # Pivot the risk strata
  df_agg |>
    mutate(year_label = paste0("yr_", year)) |>
    select(admin_2, year_label, risk_stratum) |>
    pivot_wider(names_from = year_label, values_from = risk_stratum) |>
    left_join(pop_weights, by = "admin_2") |>
    drop_na()
}

generate_sankey_data <- function(df, selected_years, weight_type = "count") {
  years <- sort(selected_years)
  req(length(years) >= 2)

  # Prep Wide Data
  wide_df <- prep_sankey_data(df, selected_years)

  # If filters result in 0 districts, return a placeholder to prevent crash
  if (is.null(wide_df) || nrow(wide_df) == 0) {
    return(NULL)
  }

  # Convert to data.table for efficient aggregation
  wide_df <- as.data.table(wide_df)

  # Create nodes
  # We need a unique ID for each year stratt
  nodes <- data.frame(
    label = character(),
    color = character(),
    total_val = numeric(),
    stringsAsFactors = FALSE
  )
  risk_cols <- get_risk_colors()

  for (yr in years) {
    col_name <- paste0("yr_", yr)
    if (!(col_name %in% colnames(wide_df))) {
      next
    }

    for (stratum in names(risk_cols)) {
      node_subset <- wide_df[wide_df[[col_name]] == stratum, ]
      val <- if (weight_type == "nHost") {
        sum(node_subset$nHost, na.rm = TRUE)
      } else {
        nrow(node_subset)
      }

      nodes <- rbind(
        nodes,
        data.table(
          label = paste0(stratum, " (", yr, ")"),
          color = unname(risk_cols[[stratum]]),
          total_val = val,
          stratum = stratum,
          year = yr
        )
      )
    }
  }

  if (nrow(nodes) == 0) {
    return(NULL)
  }
  nodes[, id := 0:(.N - 1)]

  # Build links between consecutive year pairs
  links <- data.table(
    source = integer(),
    target = integer(),
    value = numeric(),
    color = character()
  )

  for (i in 1:(length(years) - 1)) {
    year_start <- years[i]
    year_end <- years[i + 1]
    col_start <- paste0("yr_", year_start)
    col_end <- paste0("yr_", year_end)

    if (!(col_start %in% names(wide_df)) || !(col_end %in% names(wide_df))) {
      next
    }

    # Aggregate transitions between the two year columns
    transitions <- wide_df[,
      .(
        flow_value = if (weight_type == "nHost") {
          sum(nHost, na.rm = TRUE)
        } else {
          .N
        }
      ),
      by = c(col_start, col_end)
    ]
    setnames(transitions, c(col_start, col_end), c("source_lab", "target_lab"))

    for (j in seq_len(nrow(transitions))) {
      s_id <- nodes[
        stratum == transitions$source_lab[j] & year == year_start,
        id
      ]
      t_id <- nodes[stratum == transitions$target_lab[j] & year == year_end, id]

      if (length(s_id) > 0 && length(t_id) > 0) {
        links <- rbind(
          links,
          data.table(
            source = s_id,
            target = t_id,
            value = transitions$flow_value[j],
            # Link colour is a semi-transparent version of the source node colour
            color = paste0(
              unname(risk_cols[[as.character(transitions$source_lab[j])]]),
              "80"
            )
          )
        )
      }
    }
  }

  list(nodes = as.data.frame(nodes), links = as.data.frame(links))
}

#' Render Sankey diagram for risk strata transitions
#'
#' Creates a Plotly Sankey diagram showing transitions between risk strata
#' across selected years.
#'
#' @param df Data frame with risk strata data
#' @param year_range Numeric vector of length 2 (start and end year)
#' @param weight_type Weight type: "count" for district count, "nHost" for
#'   population
#' @param stepwise If TRUE, show all intermediate years; if FALSE, show only
#'   endpoints
#' @return Plotly sankey diagram object or NULL if no data
#' @export
render_sankey <- function(
  df,
  year_range,
  weight_type = "count",
  stepwise = FALSE
) {
  req(length(year_range) >= 2)

  selected_years <- if (stepwise) {
    year_range[1]:year_range[2]
  } else {
    c(year_range[1], year_range[2])
  }

  s_list <- generate_sankey_data(df, selected_years, weight_type = weight_type)

  if (is.null(s_list)) {
    return(NULL)
  }

  unit_label <- if (weight_type == "nHost") "People" else "Districts"

  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = s_list$nodes$label,
      color = s_list$nodes$color,
      pad = 15,
      thickness = 20,
      customdata = round(s_list$nodes$total_val),
      hovertemplate = paste0(
        "%{label}: %{customdata} ",
        unit_label,
        "<extra></extra>"
      )
    ),
    link = list(
      source = s_list$links$source,
      target = s_list$links$target,
      value = s_list$links$value,
      color = s_list$links$color,
      hovertemplate = paste0("Flow: %{value} ", unit_label, "<extra></extra>")
    )
  ) |>
    layout(
      font = list(size = 12),
      margin = list(l = 50, r = 50, b = 20, t = 40)
    )
}
