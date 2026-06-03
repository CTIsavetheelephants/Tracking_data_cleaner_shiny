.flag_checkbox <- function(id, label, colour, value) {
  tags$div(
    class = "d-flex align-items-center mb-0",
    tags$style("
      .flag-cb input[type='checkbox'] {
        width: 15px; height: 15px;
        border: 2px solid #555 !important;
        border-radius: 3px;
        accent-color: #555;
        cursor: pointer;
      }
      .flag-cb .form-group { margin-bottom: 0 !important; }
      .flag-cb label       { margin-bottom: 0 !important; }
    "),
    tags$span(style = paste0(
      "display:inline-block; width:12px; height:12px; border-radius:50%;",
      "background-color:", colour, "; margin-right:6px; flex-shrink:0;"
    )),
    div(class = "flag-cb", checkboxInput(id, label, value = value))
  )
}

mod_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
  tags$script(HTML("
    /* Prevent page-scroll jump when the individual dropdown is used.
     * Selecting a new individual triggers renderUI re-renders (ind_flag_summary,
     * progress_summary) whose DOM insertions can reset scroll position.
     * Fix: save scroll on focus of any select/selectize, restore after every
     * Shiny value update that arrives within 2 seconds. */
    $(document).on('focus mousedown', 'select', function() {
      window._selectScrollY    = window.scrollY || window.pageYOffset;
      window._selectScrollTime = Date.now();
    });
    $(document).on('shiny:value', function() {
      if (window._selectScrollY === undefined) return;
      if (Date.now() - (window._selectScrollTime || 0) > 2000) return;
      var pos = window._selectScrollY;
      setTimeout(function() { window.scrollTo(0, pos); }, 50);
    });
  ")),
  layout_columns(
    col_widths = c(3, 9),

    card(
      card_header("Individual"),
      selectInput(ns("individual"), "Select individual", choices = NULL),
      actionButton(ns("prev_ind"), icon("arrow-left"),  class = "btn-sm"),
      actionButton(ns("next_ind"), icon("arrow-right"), class = "btn-sm"),
      hr(),
      uiOutput(ns("ind_flag_summary")),
      hr(),
      h6("Confirm removals"),
      .flag_checkbox(ns("rm_vehicle"),    "Suspicious vehicle",  FLAG_COLOURS[["suspicious_vehicle"]],  TRUE),
      .flag_checkbox(ns("rm_airborne"),   "Suspicious airborne", FLAG_COLOURS[["suspicious_airborne"]], TRUE),
      .flag_checkbox(ns("rm_predep"),     "Pre-deployment",      FLAG_COLOURS[["predeployment"]],       TRUE),
      .flag_checkbox(ns("rm_immobile"),   "Immobility",          FLAG_COLOURS[["immobility"]],          TRUE),
      .flag_checkbox(ns("rm_shift"),      "Shift episodes",      FLAG_COLOURS[["shift_episode"]],       TRUE),
      actionButton(ns("confirm"), "Confirm & advance to next",
                   class = "btn-success w-100 mt-2"),
      actionButton(ns("undo"), "Undo removals for this individual",
                   class = "btn-outline-warning w-100 mt-1"),
      actionButton(ns("remove_individual"), "Remove individual & advance to next",
                   class = "btn-danger w-100 mt-1"),
      hr(),
      uiOutput(ns("progress_summary")),
      hr(),
      actionButton(ns("complete"), "Complete review → Step 5: Manual Cleaning",
                   class = "btn-primary w-100")
    ),

    card(
      card_header("Track review"),
      leafletOutput(ns("map"), height = "600px")
    )
  ) # close layout_columns
  ) # close tagList
}

mod_review_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    # ── Individual list ────────────────────────────────────────────────────
    observe({
      inds <- flagged_inds()
      updateSelectInput(session, "individual",
                        choices  = c("— select an individual —" = "", inds),
                        selected = "")
    })

    flagged_inds <- reactive({
      req(rv$data_flagged)
      rv$data_flagged %>%
        dplyr::filter(flag_type != "ok") %>%
        dplyr::pull(name) %>%
        unique() %>%
        sort()
    })

    observeEvent(input$prev_ind, {
      inds <- flagged_inds()
      cur  <- which(inds == input$individual)
      if (length(cur) == 0) {
        updateSelectInput(session, "individual", selected = inds[1])
      } else if (cur > 1) {
        updateSelectInput(session, "individual", selected = inds[cur - 1])
      }
    })

    observeEvent(input$next_ind, {
      inds <- flagged_inds()
      cur  <- which(inds == input$individual)
      if (length(cur) == 0) {
        updateSelectInput(session, "individual", selected = inds[1])
      } else if (cur < length(inds)) {
        updateSelectInput(session, "individual", selected = inds[cur + 1])
      }
    })

    # ── Current individual data ────────────────────────────────────────────
    ind_data <- reactive({
      req(rv$data_flagged, input$individual)
      rv$data_flagged %>%
        dplyr::filter(name == input$individual) %>%
        dplyr::arrange(timestamp_corrected)
    })

    # ── Per-individual flag summary ────────────────────────────────────────
    output$ind_flag_summary <- renderUI({
      d <- ind_data()
      counts <- table(d$flag_type)
      items  <- purrr::imap(FLAG_COLOURS, function(col, nm) {
        ct <- counts[nm]
        n  <- if (is.na(ct)) 0L else as.integer(ct)
        if (n == 0L) return(NULL)
        tags$li(
          tags$span(style = paste0("color:", col, "; font-weight:bold"), nm),
          ": ", n
        )
      })
      tags$ul(style = "padding-left:1rem; font-size:0.9rem",
              Filter(Negate(is.null), items))
    })

    # ── Map — only initialise once data is ready to avoid double-world render
    output$map <- renderLeaflet({
      req(rv$data_flagged)
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addTiles(
          "https://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
          options = tileOptions(opacity = 0.9)
        ) %>%
        addScaleBar() %>%
        addLegend(
          position = "bottomleft",
          colors   = unname(FLAG_COLOURS[c("ok", "suspicious_vehicle", "suspicious_airborne",
                                           "predeployment", "immobility", "shift_episode")]),
          labels   = c("Clean", "Suspicious vehicle", "Suspicious airborne",
                       "Pre-deployment", "Immobility", "Shift episode"),
          opacity  = 0.9,
          title    = "Flag type"
        )
    })

    observeEvent(input$individual, {
      d <- ind_data()
      req(nrow(d) > 0)

      d_pts  <- d %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          border_color = dplyr::if_else(flag_type == "ok", "#ffffff", point_color),
          fill_opacity = dplyr::if_else(flag_type == "ok", 0.6, 0.9),
          pt_radius    = dplyr::if_else(flag_type == "ok", 2L, 7L)
        ) %>%
        dplyr::select(name, timestamp_corrected, lon_wgs, lat_wgs,
                      point_color, border_color, fill_opacity, pt_radius, flag_type, step_speed_kmh)

      # Cap flagged markers; show all ok points so they match the track line
      max_flag  <- 3000L
      flagd_pts <- dplyr::filter(d_pts, flag_type != "ok")
      ok_pts    <- dplyr::filter(d_pts, flag_type == "ok")
      if (nrow(flagd_pts) > max_flag)
        flagd_pts <- flagd_pts[round(seq(1, nrow(flagd_pts), length.out = max_flag)), ]
      d_pts <- dplyr::bind_rows(flagd_pts, ok_pts)

      # Zoom to 5th–95th percentile of ALL displayed points (ok + flagged) so
      # the full track is visible without extreme outlier flags stretching the view.
      all_for_bounds <- dplyr::bind_rows(ok_pts, flagd_pts)
      lon_q <- stats::quantile(all_for_bounds$lon_wgs, c(0.05, 0.95), na.rm = TRUE)
      lat_q <- stats::quantile(all_for_bounds$lat_wgs, c(0.05, 0.95), na.rm = TRUE)

      proxy <- leafletProxy(session$ns("map"), session) %>%
        clearShapes() %>%
        clearMarkers()

      # Full track line uses all points in time order (not the display-capped subset)
      all_pts <- d %>% sf::st_drop_geometry() %>% dplyr::arrange(timestamp_corrected)
      if (nrow(all_pts) >= 2) {
        track_line <- sf::st_sfc(
          sf::st_linestring(as.matrix(all_pts[, c("lon_wgs", "lat_wgs")])),
          crs = 4326
        )
        proxy <- proxy %>%
          addPolylines(data = track_line, color = "#ffffff", weight = 1.5, opacity = 0.8)
      }

      # Points — geometry dropped so leaflet doesn't get confused by the sf column
      proxy %>%
        addCircleMarkers(
          data        = d_pts,
          lng         = ~lon_wgs,
          lat         = ~lat_wgs,
          radius      = ~pt_radius,
          color       = ~border_color,
          fillColor   = ~point_color,
          fillOpacity = ~fill_opacity,
          stroke      = TRUE,
          weight      = 1,
          popup       = ~paste0(
            "<b>", name, "</b><br>",
            format(timestamp_corrected, "%Y-%m-%d %H:%M"), "<br>",
            "Flag: ", flag_type, "<br>",
            "Speed: ", dplyr::if_else(is.na(step_speed_kmh), "—",
                                      paste0(round(step_speed_kmh, 1), " km/h"))
          )
        ) %>%
        fitBounds(
          lng1 = lon_q[[1]] - 0.05, lat1 = lat_q[[1]] - 0.05,
          lng2 = lon_q[[2]] + 0.05, lat2 = lat_q[[2]] + 0.05
        )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ── Confirm removals ───────────────────────────────────────────────────
    observeEvent(input$confirm, {
      d <- ind_data()

      flag_types_to_remove <- character(0)
      if (input$rm_vehicle)   flag_types_to_remove <- c(flag_types_to_remove, "suspicious_vehicle")
      if (input$rm_airborne)  flag_types_to_remove <- c(flag_types_to_remove, "suspicious_airborne")
      if (input$rm_predep)    flag_types_to_remove <- c(flag_types_to_remove, "predeployment")
      if (input$rm_immobile)  flag_types_to_remove <- c(flag_types_to_remove, "immobility")
      if (input$rm_shift)     flag_types_to_remove <- c(flag_types_to_remove, "shift_episode")

      new_removals <- d %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(flag_type %in% flag_types_to_remove) %>%
        dplyr::select(name, timestamp_corrected, lon, lat, flag_type)

      if (nrow(new_removals) == 0) {
        showNotification("No flagged fixes match the selected types.", type = "warning")
        return()
      }

      existing <- rv$flagged_removals
      if (is.null(existing)) {
        rv$flagged_removals <- new_removals
      } else {
        rv$flagged_removals <- dplyr::bind_rows(existing, new_removals) %>%
          dplyr::distinct(name, timestamp_corrected, .keep_all = TRUE)
      }

      showNotification(
        paste0("Confirmed ", nrow(new_removals), " removal(s) for ", input$individual, "."),
        type = "message"
      )

      # Advance to next flagged individual
      inds <- flagged_inds()
      cur  <- which(inds == input$individual)
      if (length(cur) > 0 && cur < length(inds))
        updateSelectInput(session, "individual", selected = inds[cur + 1])
    })

    # ── Undo removals for current individual ──────────────────────────────
    observeEvent(input$undo, {
      req(input$individual)
      existing <- rv$flagged_removals
      if (is.null(existing) || !input$individual %in% existing$name) {
        showNotification("No confirmed removals found for this individual.", type = "warning")
        return()
      }
      n_removed <- sum(existing$name == input$individual)
      rv$flagged_removals <- dplyr::filter(existing, name != input$individual)
      if (nrow(rv$flagged_removals) == 0) rv$flagged_removals <- NULL
      showNotification(
        paste0("Undone: ", n_removed, " removal(s) cleared for ", input$individual, "."),
        type = "message"
      )
    })

    # ── Remove entire individual ───────────────────────────────────────────
    observeEvent(input$remove_individual, {
      req(input$individual)
      all_fixes <- rv$data_flagged %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(name == input$individual) %>%
        dplyr::select(name, timestamp_corrected, lon, lat, flag_type)

      existing <- rv$flagged_removals
      rv$flagged_removals <- if (is.null(existing)) {
        all_fixes
      } else {
        dplyr::bind_rows(existing, all_fixes) %>%
          dplyr::distinct(name, timestamp_corrected, .keep_all = TRUE)
      }

      showNotification(
        paste0("All ", nrow(all_fixes), " fix(es) for '", input$individual, "' queued for removal."),
        type = "message"
      )

      inds <- flagged_inds()
      cur  <- which(inds == input$individual)
      if (length(cur) > 0 && cur < length(inds))
        updateSelectInput(session, "individual", selected = inds[cur + 1])
    })

    # ── Progress summary ───────────────────────────────────────────────────
    output$progress_summary <- renderUI({
      req(rv$data_flagged)
      total_inds    <- dplyr::n_distinct(rv$data_flagged$name)
      reviewed_inds <- if (!is.null(rv$flagged_removals))
        dplyr::n_distinct(rv$flagged_removals$name) else 0
      total_removed <- if (!is.null(rv$flagged_removals)) nrow(rv$flagged_removals) else 0
      tags$p(class = "text-muted", style = "font-size:0.85rem",
             reviewed_inds, " / ", total_inds, " individuals reviewed",
             tags$br(),
             total_removed, " fix(es) queued for removal")
    })

    # ── Complete review ────────────────────────────────────────────────────
    observeEvent(input$complete, {
      total_removed <- if (!is.null(rv$flagged_removals)) nrow(rv$flagged_removals) else 0
      showNotification(
        paste0("Review complete. ", total_removed, " fix(es) saved for removal."),
        type = "message", duration = 5
      )
      nav_select("main_nav", "Step 5: Manual Cleaning", session = parent_session)
    })

  })
}
