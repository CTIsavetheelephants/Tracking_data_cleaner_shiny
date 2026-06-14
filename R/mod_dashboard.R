mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style("
      .compact-kpi .bslib-value-box { min-height: 70px !important; }
      .compact-kpi .value-box-value { font-size: 1.25rem !important; }
      .compact-kpi .value-box-title { font-size: 0.82rem !important; }
      .compact-kpi .value-box-showcase { font-size: 1.2rem !important; }
    "),
    uiOutput(ns("kpi_boxes")),
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
    )
  )
}

mod_dashboard_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    clean <- reactive({ build_clean_data(rv) })

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
