mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style("
      .compact-kpi .bslib-value-box { min-height: 70px !important; }
      .compact-kpi .value-box-value { font-size: 1.25rem !important; }
      .compact-kpi .value-box-title { font-size: 0.82rem !important; }
      .compact-kpi .value-box-showcase { font-size: 1.2rem !important; }
      .track-legend .checkbox { margin-top: 2px !important; margin-bottom: 2px !important; }
      .track-legend label { font-weight: 400 !important; }
    "),
    uiOutput(ns("kpi_boxes")),

    # ── Individual tracks map ──────────────────────────────────────────────
    card(
      card_header("Individual tracks"),
      layout_columns(
        col_widths = c(3, 9),
        div(
          helpText(style = "font-size:0.8rem;",
                   "Tick individuals to draw their tracks. All are unticked by",
                   "default so the map loads instantly — tick only who you need."),
          div(class = "d-flex gap-1 mb-2",
              actionButton(ns("tracks_all"),  "Select all", class = "btn-sm btn-outline-primary flex-fill"),
              actionButton(ns("tracks_none"), "Clear all",  class = "btn-sm btn-outline-secondary flex-fill")),
          div(class = "track-legend",
              style = "max-height:500px; overflow-y:auto; padding-right:4px;",
              uiOutput(ns("tracks_checkboxes")))
        ),
        leafletOutput(ns("tracks_map"), height = "540px")
      )
    ),

    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Temporal coverage by individual"),
        uiOutput(ns("gantt_ui"))
      ),
      card(
        card_header("Processing summary"),
        DTOutput(ns("summary_table"))
      )
    ),
    card(
      card_header("Fix count by individual and month"),
      uiOutput(ns("heatmap_ui"))
    ),
    div(class = "d-flex justify-content-end mt-2 mb-3",
        actionButton(ns("to_export"), "Proceed to Step 7: Export →",
                     class = "btn-primary"))
  )
}

mod_dashboard_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    clean <- reactive({ build_clean_data(rv) })

    # Data used for the map / summaries — cleaned if available, else raw.
    dash_data <- reactive({ clean() %||% rv$data_raw })

    # WGS84 lon/lat for an sf object, preferring existing columns over a
    # (potentially expensive) reprojection of the geometry.
    .wgs <- function(d_sf) {
      df <- sf::st_drop_geometry(d_sf)
      if (all(c("lon_wgs", "lat_wgs") %in% names(df)))
        return(list(lon = df$lon_wgs, lat = df$lat_wgs))
      if (all(c("lon", "lat") %in% names(df)))
        return(list(lon = df$lon, lat = df$lat))
      cc <- sf::st_coordinates(sf::st_transform(d_sf, 4326))
      list(lon = cc[, 1], lat = cc[, 2])
    }

    track_inds <- reactive({
      req(rv$data_raw)
      sort(unique(dash_data()$name))
    })

    # Stable name → colour map (distinct hues) shared by legend + map.
    track_colors <- reactive({
      inds <- track_inds()
      stats::setNames(grDevices::hcl.colors(max(1, length(inds)), "Dark 3"), inds)
    })

    # ── Track legend (coloured checkboxes) ─────────────────────────────────
    output$tracks_checkboxes <- renderUI({
      inds <- track_inds()
      cols <- track_colors()
      req(length(inds) > 0)
      choice_names <- lapply(seq_along(inds), function(k)
        tags$span(
          tags$span(style = paste0("display:inline-block; width:16px; height:3px; ",
                                    "background:", cols[[k]], "; margin-right:6px; ",
                                    "vertical-align:middle;")),
          tags$span(style = "font-size:0.82rem;", inds[k])
        ))
      checkboxGroupInput(session$ns("tracks_select"), NULL,
                         choiceNames  = choice_names,
                         choiceValues = inds,
                         selected     = character(0))
    })

    observeEvent(input$tracks_all,  {
      updateCheckboxGroupInput(session, "tracks_select", selected = track_inds())
    })
    observeEvent(input$tracks_none, {
      updateCheckboxGroupInput(session, "tracks_select", selected = character(0))
    })

    # ── Base map (no tracks until an individual is ticked) ─────────────────
    output$tracks_map <- renderLeaflet({
      req(rv$data_raw)
      w   <- .wgs(dash_data())
      keep <- is.finite(w$lon) & is.finite(w$lat)
      lon_r <- range(w$lon[keep]); lat_r <- range(w$lat[keep])
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addTiles(
          "https://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
          options = tileOptions(opacity = 0.9)
        ) %>%
        addScaleBar() %>%
        fitBounds(lng1 = lon_r[[1]], lat1 = lat_r[[1]],
                  lng2 = lon_r[[2]], lat2 = lat_r[[2]])
    })

    # Draw only the ticked individuals' tracks (on demand keeps it fast).
    observeEvent(input$tracks_select, {
      proxy <- leafletProxy(session$ns("tracks_map"), session) %>%
        clearGroup("tracks")
      selv <- input$tracks_select
      if (length(selv) > 0) {
        d    <- dash_data()
        cols <- track_colors()
        for (nm in selv) {
          di <- d %>%
            dplyr::filter(name == nm) %>%
            dplyr::arrange(timestamp_corrected)
          n <- nrow(di)
          if (n < 2) next
          w   <- .wgs(di)
          idx <- if (n > 5000) round(seq(1, n, length.out = 5000)) else seq_len(n)
          proxy <- proxy %>%
            addPolylines(
              lng     = w$lon[idx],
              lat     = w$lat[idx],
              color   = cols[[nm]],
              weight  = 2,
              opacity = 0.85,
              group   = "tracks",
              label   = nm
            )
        }
      }
      proxy
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$to_export, {
      nav_select("main_nav", "Step 7: Export", session = parent_session)
    })

    # ── KPI value boxes ────────────────────────────────────────────────────
    output$kpi_boxes <- renderUI({
      req(rv$data_raw)
      raw   <- rv$data_raw
      cl    <- clean()
      fr    <- if (!is.null(rv$flagged_removals)) nrow(rv$flagged_removals) else 0
      mr    <- if (!is.null(rv$manual_removals))  nrow(rv$manual_removals)  else 0
      removed <- fr + mr
      clean_n <- if (!is.null(cl)) nrow(cl) else nrow(raw)

      med_label <- if (!is.null(rv$median_fix_interval)) paste0(rv$median_fix_interval, " hrs") else "—"

      div(class = "compact-kpi",
      layout_columns(
        col_widths = c(2, 2, 3, 3, 2),
        style = "margin-bottom: 1rem;",
        value_box(
          title    = "Individuals",
          value    = dplyr::n_distinct(raw$name),
          showcase = tags$span("\U0001F418", style = "font-size:2rem"),
          theme    = "primary"
        ),
        value_box(
          title    = "Raw fixes",
          value    = format(nrow(raw), big.mark = ","),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = "secondary"
        ),
        value_box(
          title    = "Fixes removed",
          value    = format(removed, big.mark = ","),
          showcase = bsicons::bs_icon("trash"),
          theme    = if (removed > 0) "warning" else "secondary"
        ),
        value_box(
          title    = "Clean fixes",
          value    = format(clean_n, big.mark = ","),
          showcase = bsicons::bs_icon("check-circle-fill"),
          theme    = "success"
        ),
        value_box(
          title    = "Median fix interval",
          value    = med_label,
          showcase = bsicons::bs_icon("clock-history"),
          theme    = "primary"
        )
      )) # close layout_columns + compact-kpi div
    })

    # ── Summary table ──────────────────────────────────────────────────────
    output$summary_table <- renderDT({
      req(rv$data_raw)
      compute_summary(rv)
    }, options = list(dom = "t", pageLength = 10), rownames = FALSE)

    # ── Gantt ──────────────────────────────────────────────────────────────
    output$gantt_ui <- renderUI({
      req(rv$data_raw)
      n_inds <- dplyr::n_distinct((clean() %||% rv$data_raw)$name)
      h <- paste0(max(400, n_inds * 28), "px")
      plotlyOutput(session$ns("gantt_plot"), height = h)
    })

    output$gantt_plot <- renderPlotly({
      req(rv$data_raw)
      d <- (clean() %||% rv$data_raw) %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          start   = min(timestamp_corrected, na.rm = TRUE),
          end     = max(timestamp_corrected, na.rm = TRUE),
          n_fixes = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(start) %>%
        dplyr::filter(is.finite(as.numeric(start)), is.finite(as.numeric(end)), start <= end) %>%
        dplyr::mutate(
          name  = factor(name, levels = rev(name)),
          label = paste0(
            name, "\n",
            format(start, "%Y-%m-%d"), " to ", format(end, "%Y-%m-%d"),
            "\n", format(n_fixes, big.mark = ","), " fixes"
          )
        )

      validate(need(nrow(d) > 0, "No tracking data available."))

      p <- ggplot2::ggplot(d, ggplot2::aes(
            x = start, xend = end,
            y = name,  yend = name,
            text = label
          )) +
        ggplot2::geom_segment(linewidth = 6, colour = "#3b82f6") +
        ggplot2::scale_x_datetime(date_labels = "%Y-%m", expand = ggplot2::expansion(mult = 0.02)) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_line(colour = "#e0e0e0"),
          panel.grid.minor   = ggplot2::element_blank(),
          axis.line          = ggplot2::element_line(colour = "#444444", linewidth = 0.4),
          axis.text.y        = ggplot2::element_text(size = 9),
          axis.text.x        = ggplot2::element_text(size = 9)
        )

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          height     = max(400, nrow(d) * 28),
          showlegend = FALSE,
          margin     = list(l = 10, t = 20)
        )
    })

    # ── Heatmap ────────────────────────────────────────────────────────────
    output$heatmap_ui <- renderUI({
      req(rv$data_raw)
      n_inds <- dplyr::n_distinct((clean() %||% rv$data_raw)$name)
      h <- paste0(max(300, n_inds * 25), "px")
      plotOutput(session$ns("heatmap_plot"), height = h)
    })

    output$heatmap_plot <- renderPlot({
      req(rv$data_raw)
      d <- (clean() %||% rv$data_raw) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(ym = lubridate::floor_date(timestamp_corrected, "month")) %>%
        dplyr::count(name, ym) %>%
        tidyr::complete(name, ym, fill = list(n = 0)) %>%
        dplyr::mutate(
          name = factor(name, levels = rev(sort(unique(name)))),
          n    = dplyr::if_else(n == 0L, NA_integer_, n)
        )

      ggplot(d, aes(x = ym, y = name, fill = n)) +
        geom_tile(color = "white", linewidth = 0.3) +
        scale_fill_gradient(
          low      = "#ffe030",
          high     = "#3b0764",
          name     = "Fixes",
          na.value = "white"
        ) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "bottom",
          panel.grid      = element_blank(),
          axis.line       = element_line(colour = "#444444", linewidth = 0.4),
          axis.text.y     = element_text(size = 9),
          axis.text.x     = element_text(size = 9, angle = 45, hjust = 1)
        )
    }, res = 100)


  })
}
