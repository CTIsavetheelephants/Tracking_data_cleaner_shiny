mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
  layout_columns(
    col_widths = c(6, 6),

    card(
      card_header("Removal audit files"),
      p(class = "text-muted",
        "Download the two audit CSVs that record which fixes were removed and why."),
      hr(),
      h6("Flagged removals"),
      p(class = "text-muted", style = "font-size:0.85rem",
        "Fixes confirmed during the Review step (automated flags)."),
      uiOutput(ns("flagged_info")),
      textInput(ns("fn_flagged"), "Filename", value = ""),
      downloadButton(ns("dl_flagged"), "Download flagged removals CSV",
                     class = "btn-outline-primary w-100"),
      hr(),
      h6("Manual removals"),
      p(class = "text-muted", style = "font-size:0.85rem",
        "Fixes hand-picked during the Clean step."),
      uiOutput(ns("manual_info")),
      textInput(ns("fn_manual"), "Filename", value = ""),
      downloadButton(ns("dl_manual"), "Download manual removals CSV",
                     class = "btn-outline-primary w-100")
    ),

    card(
      card_header("Cleaned dataset"),
      p(class = "text-muted",
        "Export the final cleaned sf object after all removals."),
      hr(),
      uiOutput(ns("clean_info")),
      textInput(ns("fn_rds"), "RDS filename", value = ""),
      downloadButton(ns("dl_rds"), "Download cleaned RDS",
                     class = "btn-success w-100"),
      hr(),
      textInput(ns("fn_csv"), "CSV filename", value = ""),
      downloadButton(ns("dl_csv"), "Download cleaned CSV",
                     class = "btn-outline-success w-100"),
      hr(),
      actionButton(ns("complete"), "Complete → Dashboard",
                   class = "btn-outline-primary w-100")
    )
  ),
  card(
    card_header("Summary plots"),
    p(class = "text-muted",
      "Temporal coverage and monthly fix counts for the cleaned dataset."),
    layout_columns(
      col_widths = c(6, 6),
      downloadButton(ns("dl_coverage"), "Download temporal coverage PNG",
                     class = "btn-outline-secondary w-100"),
      downloadButton(ns("dl_monthly"),  "Download fixes by month PNG",
                     class = "btn-outline-secondary w-100")
    )
  ),
  card(
    card_header("Cleaning report"),
    p(class = "text-muted",
      "A plain-text summary of every removal made during this session, broken down by individual and stage."),
    layout_columns(
      col_widths = c(3, 9),
      downloadButton(ns("dl_report"), "Download report (.txt)",
                     class = "btn-outline-secondary"),
      NULL
    ),
    hr(),
    verbatimTextOutput(ns("report_text"))
  )
  ) # close tagList
}

mod_export_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    area_slug <- reactive({
      req(rv$study_area)
      gsub("\\s+", "_", trimws(rv$study_area))
    })

    clean <- reactive({ build_clean_data(rv) })

    # ── Populate default filenames whenever study area changes ─────────────
    observeEvent(rv$study_area, {
      req(rv$study_area)
      s <- area_slug()
      updateTextInput(session, "fn_flagged", value = paste0(s, "_flagged_removals.csv"))
      updateTextInput(session, "fn_manual",  value = paste0(s, "_manual_removals.csv"))
      updateTextInput(session, "fn_rds",     value = paste0(s, "_clean.rds"))
      updateTextInput(session, "fn_csv",     value = paste0(s, "_clean.csv"))
    })

    # ── Info panels ────────────────────────────────────────────────────────
    output$flagged_info <- renderUI({
      n <- if (!is.null(rv$flagged_removals)) nrow(rv$flagged_removals) else 0
      tags$p(class = "text-muted", style = "font-size:0.85rem", n, " fix(es)")
    })

    output$manual_info <- renderUI({
      n <- if (!is.null(rv$manual_removals)) nrow(rv$manual_removals) else 0
      tags$p(class = "text-muted", style = "font-size:0.85rem", n, " fix(es)")
    })

    output$clean_info <- renderUI({
      d <- clean()
      if (is.null(d)) return(tags$p(class = "text-muted", "No data yet."))
      tags$p(class = "text-muted", style = "font-size:0.85rem",
             dplyr::n_distinct(d$name), " individuals | ", nrow(d), " fixes")
    })

    # ── Downloads ──────────────────────────────────────────────────────────
    output$dl_flagged <- downloadHandler(
      filename = function() if (nzchar(input$fn_flagged)) input$fn_flagged else paste0(area_slug(), "_flagged_removals.csv"),
      content  = function(file) {
        d <- rv$flagged_removals
        if (is.null(d)) d <- tibble::tibble(
          name = character(), timestamp_corrected = as.POSIXct(character()),
          lon = numeric(), lat = numeric(), flag_type = character()
        )
        readr::write_csv(d, file)
      }
    )

    output$dl_manual <- downloadHandler(
      filename = function() if (nzchar(input$fn_manual)) input$fn_manual else paste0(area_slug(), "_manual_removals.csv"),
      content  = function(file) {
        d <- rv$manual_removals
        if (is.null(d)) d <- tibble::tibble(
          name = character(), timestamp_corrected = as.POSIXct(character()),
          lon = numeric(), lat = numeric(), flag_type = character()
        )
        readr::write_csv(d, file)
      }
    )

    flag_drop_cols <- c("speed_flag", "predeployment_auto", "shift_episode",
                        "immobility", "hq_flag", "outside_bbox",
                        "point_color", "flag_type")

    output$dl_rds <- downloadHandler(
      filename = function() if (nzchar(input$fn_rds)) input$fn_rds else paste0(area_slug(), "_clean.rds"),
      content  = function(file) {
        req(clean())
        clean() %>%
          dplyr::select(-dplyr::any_of(flag_drop_cols)) %>%
          saveRDS(file)
      }
    )

    output$dl_csv <- downloadHandler(
      filename = function() if (nzchar(input$fn_csv)) input$fn_csv else paste0(area_slug(), "_clean.csv"),
      content  = function(file) {
        req(clean())
        clean() %>%
          dplyr::select(-dplyr::any_of(flag_drop_cols)) %>%
          sf::st_drop_geometry() %>%
          readr::write_csv(file)
      }
    )

    # ── Summary plots ──────────────────────────────────────────────────────
    output$dl_coverage <- downloadHandler(
      filename = function() paste0(area_slug(), "_temporal_coverage.png"),
      content  = function(file) {
        req(clean())
        d <- clean() %>% sf::st_drop_geometry()
        coverage <- d %>%
          dplyr::group_by(name) %>%
          dplyr::summarise(
            start   = min(timestamp_corrected, na.rm = TRUE),
            end     = max(timestamp_corrected, na.rm = TRUE),
            n_fixes = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::arrange(start)
        p <- ggplot2::ggplot(coverage,
               ggplot2::aes(y = reorder(name, start))) +
          ggplot2::geom_segment(
            ggplot2::aes(x = start, xend = end, yend = reorder(name, start)),
            linewidth = 2.5, colour = "#2c7bb6", lineend = "round") +
          ggplot2::geom_point(ggplot2::aes(x = start),
            size = 2.5, colour = "#2c7bb6") +
          ggplot2::geom_point(ggplot2::aes(x = end),
            size = 2.5, colour = "#2c7bb6") +
          ggplot2::geom_text(
            ggplot2::aes(x = start,
                         label = paste0(" ", n_fixes, " fixes")),
            hjust = 0, size = 2.5, colour = "grey40") +
          ggplot2::scale_x_datetime(
            date_labels = "%Y", date_breaks = "1 year",
            expand = ggplot2::expansion(mult = 0.02)) +
          ggplot2::labs(
            title = paste0(rv$study_area, " — temporal coverage by individual"),
            x = NULL, y = NULL) +
          ggplot2::theme_minimal(base_size = 10) +
          ggplot2::theme(
            axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1),
            panel.grid.major.x = ggplot2::element_line(colour = "grey88"),
            panel.grid.major.y = ggplot2::element_line(colour = "grey92"),
            panel.grid.minor.x = ggplot2::element_blank())
        ggplot2::ggsave(file, p,
          width  = 14,
          height = max(5, nrow(coverage) * 0.28),
          dpi    = 150, limitsize = FALSE)
      }
    )

    output$dl_monthly <- downloadHandler(
      filename = function() paste0(area_slug(), "_fixes_by_month.png"),
      content  = function(file) {
        req(clean())
        d <- clean() %>% sf::st_drop_geometry()
        monthly <- d %>%
          dplyr::mutate(
            month = lubridate::floor_date(timestamp_corrected, "month")) %>%
          dplyr::group_by(name, month) %>%
          dplyr::summarise(n_fixes = dplyr::n(), .groups = "drop")
        n_months <- dplyr::n_distinct(monthly$month)
        n_inds   <- dplyr::n_distinct(monthly$name)
        p <- ggplot2::ggplot(monthly,
               ggplot2::aes(x = month,
                            y = reorder(name, month, min),
                            fill = n_fixes)) +
          ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
          ggplot2::scale_fill_viridis_c(
            name = "Fixes/month", option = "plasma", direction = -1) +
          ggplot2::scale_x_datetime(
            date_labels = "%b\n%Y", date_breaks = "3 months") +
          ggplot2::labs(
            title = paste0(rv$study_area, " — fix count by individual and month"),
            x = NULL, y = NULL) +
          ggplot2::theme_minimal(base_size = 9) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 7),
            axis.text.y = ggplot2::element_text(size = 7),
            legend.position = "right")
        ggplot2::ggsave(file, p,
          width  = max(10, n_months * 0.35),
          height = max(5,  n_inds   * 0.3),
          dpi    = 150, limitsize = FALSE)
      }
    )

    observeEvent(input$complete, {
      nav_select("main_nav", "Dashboard", session = parent_session)
    })

    # ── Cleaning report ────────────────────────────────────────────────────
    report_text <- reactive({
      req(rv$data_raw)
      build_cleaning_report_text(rv)
    })

    output$report_text <- renderText({
      report_text()
    })

    output$dl_report <- downloadHandler(
      filename = function() paste0(area_slug(), "_cleaning_report.txt"),
      content  = function(file) writeLines(report_text(), file)
    )

  })
}
