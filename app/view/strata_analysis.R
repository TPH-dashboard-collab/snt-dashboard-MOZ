# fmt: skip
box::use(
  config,
  DT[DTOutput, datatable, formatPercentage, renderDT],
  bslib,
  config,
  data.table[`:=`, `.N`,  as.data.table, data.table, dcast, setorder, uniqueN],
  DT[DTOutput, datatable, renderDT, formatPercentage],
  leaflet[leafletOutput, renderLeaflet],
  logger[log_debug],
  plotly[layout, plot_ly, plotlyOutput, renderPlotly],
  rlang[`%||%`],
  sf[st_as_sf, st_bbox, st_geometry],
  shiny,
  utils[head],
)

# fmt: skip
box::use(
  app/logic/analytics/strata[
    aggregate_strata_data,
    prep_heatmap_data,
    prep_persistence_data,
  ],
  app/logic/core/logging[log_ns_builder, log_ns_fn],
  app/logic/core/varfns[classify_risk, high_risk_strata, risk_levels],
  app/logic/visualization/color_palettes[get_risk_colors],
  app/logic/visualization/maps[add_strata_map, create_map_labels],
  app/logic/visualization/sankey[prep_sankey_data, render_sankey],
)

#' Creates the UI component for risk strata analysis
#'
#' @param id The module ID used for namespacing
#' @return A Shiny UI component containing the strata analysis tabs
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$navset_card_pill(
    id = ns("strata_tabs"),
    title = "Risk Strata Movement Analysis",
    bslib$nav_panel(
      "Strata Map",
      shiny$uiOutput(ns("summary_map")),
      shiny$hr(),
      bslib$card(
        full_screen = TRUE,
        bslib$card_header("Spatial Distribution of Risk Strata"),
        leafletOutput(ns("strata_map"), height = "500px")
      )
    ),
    bslib$nav_panel(
      "Strata Movement",
      shiny$uiOutput(ns("summary_sankey")),
      shiny$hr(),
      bslib$card(
        full_screen = TRUE,
        bslib$card_header(
          shiny$div(
            style = paste0(
              "display:flex; justify-content:space-between; ",
              "align-items:center; width:100%;"
            ),
            shiny$span("Risk Strata Movement (Start to End)"),
            shiny$div(
              style = "display:flex; align-items:center; gap:12px;",
              shiny$radioButtons(
                ns("sankey_weight"),
                label = NULL,
                choices = list(
                  "District Count" = "count",
                  "Population" = "nHost"
                ),
                selected = "count",
                inline = TRUE
              )
            )
          )
        ),
        plotlyOutput(ns("sankey_plot"), height = "500px")
      )
    ),
    bslib$nav_panel(
      "Persistence Heatmap",
      shiny$uiOutput(ns("summary_heatmap")),
      shiny$hr(),
      bslib$card(
        full_screen = TRUE,
        bslib$card_header(
          shiny$div(
            style = paste0(
              "display:flex; justify-content:space-between; ",
              "align-items:center; width:100%;"
            ),
            shiny$span("District Risk Strata Over Time"),
            shiny$div(
              style = "display:flex; align-items:center; gap:8px;",
              shiny$tags$label(
                "Show districts:",
                style = paste0(
                  "font-size:0.85rem; margin-bottom:0; ",
                  "white-space:nowrap; color:#495057;"
                )
              ),
              shiny$selectInput(
                ns("heatmap_top_n"),
                label = NULL,
                choices = c(
                  "Top 5" = 5,
                  "Top 10" = 10,
                  "Top 20" = 20,
                  "Top 50" = 50,
                  "All" = 9999
                ),
                selected = 9999,
                width = "110px"
              )
            )
          )
        ),
        plotlyOutput(ns("heatmap_plot"), height = "600px")
      )
    ),
    bslib$nav_panel(
      "Persistence Table",
      shiny$uiOutput(ns("summary_persistence")),
      shiny$hr(),
      bslib$card(
        full_screen = TRUE,
        bslib$card_header("Districts Remaining in High/Moderate Strata"),
        DTOutput(ns("persistence_table"))
      )
    )
  )
}

#' Creates the server logic for risk strata analysis
#'
#' @param id The module ID
#' @param variables Application variables
#' @param country_map_dt Geographic data
#' @param use_custom_data Whether to use custom scenario data
#' @param trigger Reactive trigger for updates
#' @param ... Additional arguments including log_ns
#' @export
server <- function(
  id,
  variables,
  country_map_dt,
  use_custom_data = FALSE,
  trigger,
  ...
) {
  shiny$moduleServer(id, function(input, output, session) {
    args <- list(...)
    log_ns <- log_ns_builder(args, "strata_analysis")

    # Reactive for aggregated strata data
    strata_data <- shiny$reactive({
      if (use_custom_data) {
        shiny$req(
          variables$session_state$custom_data,
          ncol(variables$session_state$custom_data) > 1
        )
        variables$custom_data_trigger()
      } else {
        trigger()
      }

      shiny$req(
        variables$session_state$age_group,
        variables$session_state$year_start,
        variables$session_state$year_end
      )

      log_debug("Fetching strata data...", namespace = log_ns_fn(log_ns))

      # Build filters
      filters <- list(
        age_group = variables$session_state$age_group,
        plan = unname(unlist(lapply(config$get("plans"), names)))
      )
      if (
        variables$session_state$agg_level == config$get("aggregation_levels")[2] &&
          length(variables$session_state$region_selected) > 0
      ) {
        filters <- c(
          filters,
          list(admin_1 = variables$session_state$region_selected)
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

      # Aggregate and calculate risk strata using data.table
      agg_data <- aggregate_strata_data(
        dt,
        age_filter = variables$session_state$age_group
      )

      log_debug("Strata data aggregated", namespace = log_ns_fn(log_ns))
      as.data.table(agg_data)
    })

    # Year range reactive
    year_range <- shiny$reactive({
      shiny$req(
        variables$session_state$year_start,
        variables$session_state$year_end
      )
      c(
        variables$session_state$year_start,
        variables$session_state$year_end
      )
    })

    # Selected years for analysis
    selected_years <- shiny$reactive({
      yr <- year_range()
      yr[1]:yr[2]
    })

    ## SHARED SUMMARY CALCULATIONS ##
    # Central reactive so all tabs draw from the same numbers
    summary_stats <- shiny$reactive({
      shiny$req(strata_data(), year_range())

      dt <- strata_data()
      year_start <- year_range()[1]
      year_end <- year_range()[2]

      # District counts per stratum at end year
      # Aggregate across plans to get ONE row per district (avoids double-counting)
      end_year_dt <- dt[
        year == year_end,
        .(prevalenceRate = mean(prevalenceRate, na.rm = TRUE)),
        by = .(admin_1, admin_2)
      ]
      # Recalculate risk_stratum based on aggregated prevalence
      end_year_dt[, risk_stratum := classify_risk(prevalenceRate)]
      total_districts <- nrow(end_year_dt)
      high_count <- end_year_dt[risk_stratum == "High", .N]
      mod_count <- end_year_dt[risk_stratum == "Moderate", .N]
      low_count <- end_year_dt[risk_stratum %in% c("Low", "Very Low"), .N]

      # Persistence: pivot to wide across the full selected range
      wide_dt <- prep_sankey_data(dt, year_start:year_end)
      first_col <- paste0("yr_", year_start)
      last_col <- paste0("yr_", year_end)

      # Initialize values
      persist_n <- 0L
      persist_perc <- 0
      improved_n <- 0L
      declined_to_high <- 0L
      declined_to_mod <- 0L

      if (
        !is.null(wide_dt) &&
          nrow(wide_dt) > 0 &&
          first_col %in% names(wide_dt) &&
          last_col %in% names(wide_dt)
      ) {
        wide_dt <- as.data.table(wide_dt)
        total_w <- nrow(wide_dt)

        persist_n <- wide_dt[
          get(first_col) %in%
            high_risk_strata() &
            get(last_col) %in% high_risk_strata(),
          .N
        ]

        improved_n <- wide_dt[
          get(first_col) %in%
            high_risk_strata() &
            get(last_col) %in% c("Low", "Very Low"),
          .N
        ]

        persist_perc <- if (total_w > 0) {
          round((persist_n / total_w) * 100, 1)
        } else {
          0
        }

        # Districts that worsened INTO High
        declined_to_high <- wide_dt[
          get(first_col) != "High" & get(last_col) == "High",
          .N
        ]

        # Districts that worsened INTO Moderate
        declined_to_mod <- wide_dt[
          get(first_col) %in%
            c("Low", "Very Low") &
            get(last_col) == "Moderate",
          .N
        ]
      }

      # Mean prevalence change start -> end (aggregate by district first)
      start_agg <- dt[
        year == year_start,
        .(prevalenceRate = mean(prevalenceRate, na.rm = TRUE)),
        by = admin_2
      ]
      end_agg <- dt[
        year == year_end,
        .(prevalenceRate = mean(prevalenceRate, na.rm = TRUE)),
        by = admin_2
      ]
      med_change <- round(
        (mean(end_agg$prevalenceRate, na.rm = TRUE) -
          mean(start_agg$prevalenceRate, na.rm = TRUE)) *
          100,
        2
      )

      list(
        total_districts = total_districts,
        high_count = high_count,
        mod_count = mod_count,
        low_count = low_count,
        persist_n = persist_n,
        persist_perc = persist_perc,
        improved_n = improved_n,
        declined_to_high = declined_to_high,
        declined_to_mod = declined_to_mod,
        med_change = med_change,
        year_start = year_start,
        year_end = year_end
      )
    })

    ## TAB 1: RISK MAP SUMMARY ##
    output$summary_map <- shiny$renderUI({
      shiny$req(summary_stats())
      s <- summary_stats()
      shiny$tagList(
        shiny$fluidRow(
          shiny$column(3, value_box_ui(s$total_districts, "Total Districts")),
          shiny$column(
            3,
            value_box_ui(
              s$high_count,
              paste0("High Risk (", s$year_end, ")"),
              "vb-danger"
            )
          ),
          shiny$column(
            3,
            value_box_ui(
              s$mod_count,
              paste0("Moderate Risk (", s$year_end, ")"),
              "vb-warning"
            )
          ),
          shiny$column(
            3,
            value_box_ui(
              s$low_count,
              paste0("Low / Very Low (", s$year_end, ")"),
              "vb-success"
            )
          )
        ),
        shiny$fluidRow(
          shiny$column(
            12,
            alert_ui(
              paste0(
                "<b>Snapshot (",
                s$year_end,
                "):</b> Of <b>",
                s$total_districts,
                "</b> districts, <b>",
                s$high_count + s$mod_count,
                "</b> remain in High or Moderate risk strata. ",
                "Mean prevalence change since <b>",
                s$year_start,
                "</b>: ",
                ifelse(s$med_change >= 0, "+", ""),
                s$med_change,
                " pp."
              ),
              type = if (s$med_change < 0) "success" else "warning"
            )
          )
        )
      )
    })

    ## TAB 2: SANKEY SUMMARY ##
    output$summary_sankey <- shiny$renderUI({
      shiny$req(summary_stats())
      s <- summary_stats()
      shiny$tagList(
        shiny$fluidRow(
          shiny$column(
            3,
            value_box_ui(
              s$declined_to_high,
              "Declined to High Risk",
              "vb-danger"
            )
          ),
          shiny$column(
            3,
            value_box_ui(
              s$declined_to_mod,
              "Declined to Moderate Risk",
              "vb-warning"
            )
          ),
          shiny$column(
            3,
            value_box_ui(s$improved_n, "Improved to Low/Very Low", "vb-success")
          ),
          shiny$column(
            3,
            value_box_ui(s$persist_n, "Stayed High/Moderate", "vb-danger")
          )
        ),
        shiny$fluidRow(
          shiny$column(
            12,
            alert_ui(
              paste0(
                "<b>Flow Summary (",
                s$year_start,
                " \u2192 ",
                s$year_end,
                "):</b> ",
                "<b>",
                s$declined_to_high,
                "</b> district(s) worsened into ",
                "High risk and ",
                "<b>",
                s$declined_to_mod,
                "</b> declined into Moderate. ",
                "<b>",
                s$improved_n,
                "</b> improved out of elevated strata."
              ),
              type = if (
                (s$declined_to_high + s$declined_to_mod) > s$improved_n
              ) {
                "danger"
              } else {
                "warning"
              }
            )
          )
        )
      )
    })

    ## TAB 3: HEATMAP SUMMARY ##
    output$summary_heatmap <- shiny$renderUI({
      shiny$req(summary_stats(), strata_data(), year_range())
      s <- summary_stats()
      year_start <- year_range()[1]
      year_end <- year_range()[2]
      n_yr <- length(year_start:year_end)

      # Districts persistently High/Moderate for ALL years in the range
      heatmap_dt <- prep_heatmap_data(strata_data(), year_start:year_end)
      always_high <- 0L
      if (!is.null(heatmap_dt)) {
        heatmap_dt <- as.data.table(heatmap_dt)
        always_high <- heatmap_dt[,
          .(all_high = all(is_high_mod)),
          by = admin_2
        ][all_high == TRUE, .N]
      }

      shiny$tagList(
        shiny$fluidRow(
          shiny$column(
            3,
            value_box_ui(s$persist_n, "Moderate/High Districts", "vb-danger")
          ),
          shiny$column(
            3,
            value_box_ui(
              always_high,
              paste0("Elevated All ", n_yr, " Years"),
              "vb-danger"
            )
          ),
          shiny$column(
            3,
            value_box_ui(
              paste0(s$persist_perc, "%"),
              "Persistence Rate",
              "vb-warning"
            )
          ),
          shiny$column(3, value_box_ui(n_yr, "Years in Period"))
        ),
        shiny$fluidRow(
          shiny$column(
            12,
            alert_ui(
              paste0(
                "<b>Heatmap guide:</b> Districts are ordered top-to-bottom by ",
                "the number of years ",
                "spent in High or Moderate strata. <b>",
                always_high,
                "</b> district(s) remained elevated for the entire <b>",
                s$year_start,
                "\u2013",
                s$year_end,
                "</b> period."
              ),
              type = "info"
            )
          )
        )
      )
    })

    ## TAB 4: PERSISTENCE TABLE SUMMARY ##
    output$summary_persistence <- shiny$renderUI({
      shiny$req(summary_stats())
      s <- summary_stats()
      shiny$tagList(
        shiny$fluidRow(
          shiny$column(
            4,
            value_box_ui(
              s$persist_n,
              "Persistently Elevated Districts",
              "vb-danger"
            )
          ),
          shiny$column(
            4,
            value_box_ui(
              paste0(s$persist_perc, "%"),
              "% of All Districts",
              "vb-warning"
            )
          ),
          shiny$column(
            4,
            value_box_ui(s$improved_n, "Districts That Improved", "vb-success")
          )
        ),
        shiny$fluidRow(
          shiny$column(
            12,
            alert_ui(
              paste0(
                "<b>Persistence (",
                s$year_start,
                "\u2013",
                s$year_end,
                "):</b> ",
                "Districts listed below started <em>and</em> ended in High or ",
                "Moderate strata. ",
                "These <b>",
                s$persist_n,
                "</b> districts should be prioritised",
                " for intensified intervention."
              ),
              type = "danger"
            )
          )
        )
      )
    })

    ## SANKEY PLOT ##
    output$sankey_plot <- renderPlotly({
      shiny$req(strata_data(), year_range())
      yrs <- year_range()

      # Validate that year range spans at least 2 different years
      if (yrs[1] == yrs[2]) {
        return(
          plot_ly() |>
            layout(
              annotations = list(
                list(
                  text = "Select different start and end years to view strata movement",
                  x = 0.5,
                  y = 0.5,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 16, color = "#666")
                )
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      log_debug("Rendering sankey plot...", namespace = log_ns_fn(log_ns))

      render_sankey(
        df = strata_data(),
        year_range = yrs,
        weight_type = input$sankey_weight %||% "count",
        stepwise = FALSE
      )
    })

    ## STRATA MAP ##
    output$strata_map <- renderLeaflet({
      shiny$req(strata_data(), year_range())
      dt <- strata_data()
      yrs <- year_range()
      yr_start <- yrs[1]
      yr_end <- yrs[2]

      log_debug("Rendering strata map...", namespace = log_ns_fn(log_ns))

      # Prepare wide format for map
      # Aggregate to handle multiple plans - take mean prevalence per district/year
      dt_filtered <- dt[
        year %in% c(yr_start, yr_end),
        .(prevalenceRate = mean(prevalenceRate, na.rm = TRUE)),
        by = .(admin_1, admin_2, year)
      ]

      # Recalculate risk_stratum based on aggregated prevalence
      dt_filtered[, risk_stratum := classify_risk(prevalenceRate)]

      # Pivot wide using data.table dcast
      wide_dt <- dcast(
        dt_filtered,
        admin_1 + admin_2 ~ year,
        value.var = c("risk_stratum", "prevalenceRate")
      )

      # Merge with geographic data
      geo_data <- merge(
        wide_dt,
        country_map_dt,
        by = c("admin_1", "admin_2")
      )
      geo_data <- st_as_sf(geo_data)

      # Create map labels
      labels <- create_map_labels(geo_data, yr_start, yr_end)

      # Calculate map bounds
      map_bounds <- as.data.table(
        do.call("rbind", lapply(st_geometry(geo_data), st_bbox))
      )
      map_bounds <- list(
        xmin = min(map_bounds[["xmin"]]),
        ymin = min(map_bounds[["ymin"]]),
        xmax = max(map_bounds[["xmax"]]),
        ymax = max(map_bounds[["ymax"]])
      )

      # Risk color column for end year
      risk_col <- paste0("risk_stratum_", yr_end)
      risk_colors <- get_risk_colors()

      add_strata_map(
        geo_data = geo_data,
        risk_col = risk_col,
        risk_colors = risk_colors,
        map_bounds = map_bounds,
        labels = labels,
        legend_title = paste0("Risk Strata (", yr_end, ")")
      )
    })

    ## PERSISTENCE HEATMAP ##
    output$heatmap_plot <- renderPlotly({
      shiny$req(strata_data(), year_range())
      dt <- strata_data()
      yrs <- selected_years()

      log_debug("Rendering heatmap...", namespace = log_ns_fn(log_ns))

      heatmap_df <- prep_heatmap_data(dt, yrs)
      if (is.null(heatmap_df)) {
        return(NULL)
      }

      # Apply top N filter
      top_n <- as.numeric(input$heatmap_top_n %||% 9999)
      heatmap_dt <- as.data.table(heatmap_df)
      if (top_n < 9999) {
        # Get top N districts by persistence in High/Moderate
        # Sort all districts by persist_years
        district_summary <- unique(heatmap_dt[, .(admin_2, persist_years)])
        setorder(district_summary, -persist_years)
        top_districts <- head(district_summary$admin_2, top_n)
        heatmap_df <- heatmap_df[heatmap_df$admin_2 %in% top_districts, ]
        # Drop unused factor levels so plotly only shows filtered districts
        heatmap_df$admin_2 <- droplevels(heatmap_df$admin_2)
      }

      if (nrow(heatmap_df) == 0) {
        return(NULL)
      }

      risk_colors <- get_risk_colors()
      n_districts <- uniqueN(heatmap_dt$admin_2)
      row_height  <- max(18, min(35, 600 / max(n_districts, 1)))
      # Create heatmap using plotly
      plot_ly(
        data = heatmap_df,
        x = ~year,
        y = ~admin_2,
        z = ~ as.numeric(factor(
          risk_stratum,
          levels = risk_levels()
        )),
        type = "heatmap",
        colors = unname(risk_colors),
        colorscale = list(
          list(0, risk_colors["Very Low"]),
          list(0.33, risk_colors["Low"]),
          list(0.66, risk_colors["Moderate"]),
          list(1, risk_colors["High"])
        ),
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "Year: %{x}<br>",
          "Risk: %{text}<br>",
          "Prevalence: %{customdata:.2%}",
          "<extra></extra>"
        ),
        text = ~risk_stratum,
        customdata = ~prevalenceRate,
        showscale = FALSE,
        xgap = 2,
        ygap = 2
      ) |>
      layout(
            xaxis = list(
              title    = "Year",
              dtick    = 1,
              tickmode = "linear",
              showgrid = FALSE,
              zeroline = FALSE
            ),
            yaxis = list(
              title    = "",
              tickfont = list(size = min(11, max(8, row_height * 0.55)), color = "black"),
              showgrid = FALSE,
              zeroline = FALSE
          ),
          margin = list(l = 120, r = 20, t = 40, b = 60),
          plot_bgcolor  = "#f0f0f0",
          paper_bgcolor = "white"
        )
    })

    ## PERSISTENCE TABLE ##
    output$persistence_table <- renderDT({
      shiny$req(strata_data(), year_range())
      dt <- strata_data()
      yrs <- year_range()

      log_debug("Rendering persistence table...", namespace = log_ns_fn(log_ns))

      persistence_dt <- prep_persistence_data(dt, yrs)

      if (is.null(persistence_dt) || nrow(persistence_dt) == 0) {
        return(
          datatable(
            data.table(Message = "No persistent districts found."),
            options = list(dom = "t")
          )
        )
      }

      yr_start <- yrs[1]
      yr_end <- yrs[2]

      # Format column names for display
      col_s_risk <- paste0("risk_stratum_", yr_start)
      col_e_risk <- paste0("risk_stratum_", yr_end)
      col_s_prev <- paste0("prevalenceRate_", yr_start)
      col_e_prev <- paste0("prevalenceRate_", yr_end)

      display_dt <- data.table(
        Region = persistence_dt$admin_1,
        District = persistence_dt$admin_2,
        `Start Risk` = persistence_dt[[col_s_risk]],
        `End Risk` = persistence_dt[[col_e_risk]],
        `Start PfPR` = persistence_dt[[col_s_prev]],
        `End PfPR` = persistence_dt[[col_e_prev]],
        `PfPR Change` = persistence_dt$PfPR_Change
      )

      datatable(
        display_dt,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "frtip"
        ),
        rownames = FALSE,
        filter = "top"
      ) |>
        formatPercentage(c("Start PfPR", "End PfPR", "PfPR Change"), 1)
    })
  })
}

# Helper function for value box UI
value_box_ui <- function(value, label, class = "") {
  shiny$div(
    class = paste("summary-valuebox", class),
    shiny$div(class = "vb-value", value),
    shiny$div(class = "vb-label", label)
  )
}

# Helper function for alert UI
alert_ui <- function(message, type = "info") {
  alert_class <- paste0("alert alert-", type)
  shiny$div(
    class = alert_class,
    shiny$HTML(message)
  )
}
