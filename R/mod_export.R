mod_export_ui <- function(id) {
  ns <- NS(id)
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
  )
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

    observeEvent(input$complete, {
      nav_select("main_nav", "Dashboard", session = parent_session)
    })

  })
}
