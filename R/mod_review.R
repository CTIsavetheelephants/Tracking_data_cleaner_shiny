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

# Colour used to highlight fixes staged for manual removal on the review map.
MANUAL_SEL_COLOUR <- "#ff00ff"

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
    col_widths = c(5, 7),

    card(
      card_header("Review & clean individual"),
      selectInput(ns("individual"), "Select individual", choices = NULL),
      div(class = "d-flex gap-1 mb-1",
          actionButton(ns("prev_ind"), icon("arrow-left"),  class = "btn-sm"),
          actionButton(ns("next_ind"), icon("arrow-right"), class = "btn-sm")),
      uiOutput(ns("ind_flag_summary")),

      layout_columns(
        col_widths = c(6, 6),

        # ── Left sub-column: flag removals ────────────────────────────────
        div(
          h6("Confirm flag removals"),
          .flag_checkbox(ns("rm_vehicle"),  "Possible vehicle",  FLAG_COLOURS[["suspicious_vehicle"]],  TRUE),
          .flag_checkbox(ns("rm_airborne"), "Possible airborne", FLAG_COLOURS[["suspicious_airborne"]], TRUE),
          .flag_checkbox(ns("rm_predep"),   "Pre-deployment",    FLAG_COLOURS[["predeployment"]],       TRUE),
          .flag_checkbox(ns("rm_immobile"), "Immobility",        FLAG_COLOURS[["immobility"]],          TRUE),
          .flag_checkbox(ns("rm_shift"),    "Shift episodes",    FLAG_COLOURS[["shift_episode"]],       TRUE)
        ),

        # ── Right sub-column: manual point removal ────────────────────────
        div(
          h6("Manual point removal"),
          helpText(style = "font-size:0.78rem;",
                   "Click points, or draw a box on the map, to stage extra fixes",
                   "(shown in", tags$b(style = paste0("color:", MANUAL_SEL_COLOUR), "magenta"),
                   "). They are removed together with the checked flags when you",
                   "press Confirm."),
          uiOutput(ns("selected_info")),
          actionButton(ns("select_before"), "Select points before",
                       class = "btn-outline-warning btn-sm w-100 mt-1"),
          actionButton(ns("select_after"),  "Select points after",
                       class = "btn-outline-warning btn-sm w-100 mt-1"),
          actionButton(ns("clear_selected"), "Clear manual selection",
                       class = "btn-outline-secondary btn-sm w-100 mt-1")
        )
      ),

      actionButton(ns("confirm"), "Confirm & advance to next",
                   class = "btn-success w-100 mt-2"),
      div(class = "d-flex gap-1 mt-1",
          actionButton(ns("undo"), "Undo removals (this individual)",
                       class = "btn-outline-warning btn-sm flex-fill"),
          actionButton(ns("remove_individual"), "Remove whole individual",
                       class = "btn-danger btn-sm flex-fill")),
      hr(),
      uiOutput(ns("progress_summary")),
      actionButton(ns("complete"), "Complete review → Step 5: Manual Cleaning",
                   class = "btn-primary w-100 mt-1")
    ),

    card(
      card_header("Track review — draw a box or click points to remove multiple fixes"),
      p(class = "text-muted", style = "font-size:0.82rem; margin-bottom:0.4rem",
        "Coloured points show the automatic flags (see legend). To remove fixes the",
        "flags missed: click a point, or click the", tags$b("rectangle icon"),
        "top-left and drag a box — staged fixes turn",
        tags$b(style = paste0("color:", MANUAL_SEL_COLOUR), "magenta"),
        "and are removed when you press Confirm."),
      leafletOutput(ns("map"), height = "600px")
    )
  ) # close layout_columns
  ) # close tagList
}

mod_review_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    sel <- reactiveVal(character(0))  # staged manual point IDs (name|timestamp)

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

    # ── Current individual data (all fixes, no removals applied) ───────────
    ind_data <- reactive({
      req(rv$data_flagged, input$individual)
      rv$data_flagged %>%
        dplyr::filter(name == input$individual) %>%
        dplyr::arrange(timestamp_corrected)
    })

    # Flat table with a stable per-point id, used for selection + drawing.
    d_pts_data <- reactive({
      d <- ind_data()
      req(nrow(d) > 0)
      d %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(point_id = paste0(name, "|",
                                        format(timestamp_corrected, "%Y-%m-%d %H:%M:%S"))) %>%
        dplyr::select(name, timestamp_corrected, lon_wgs, lat_wgs, point_id,
                      flag_type, point_color, step_speed_kmh)
    })

    # Point IDs currently staged/committed as manual removals for an individual.
    manual_ids_for <- function(indiv) {
      mr <- rv$manual_removals
      if (is.null(mr) || nrow(mr) == 0) return(character(0))
      mr <- dplyr::filter(mr, name == indiv)
      if (nrow(mr) == 0) return(character(0))
      paste0(mr$name, "|", format(mr$timestamp_corrected, "%Y-%m-%d %H:%M:%S"))
    }

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
      tags$ul(style = "padding-left:1rem; font-size:0.85rem; margin-bottom:0.3rem",
              Filter(Negate(is.null), items))
    })

    # ── Marker drawing helper (shared by full render + light redraw) ───────
    .draw_points <- function(proxy, d_pts, selected_ids) {
      draw <- d_pts %>%
        dplyr::mutate(
          is_sel   = point_id %in% selected_ids,
          fill_col = dplyr::if_else(is_sel, MANUAL_SEL_COLOUR, point_color),
          brd_col  = dplyr::if_else(is_sel, "#ffffff",
                       dplyr::if_else(flag_type == "ok", "#ffffff", point_color)),
          opac     = dplyr::if_else(is_sel, 1,
                       dplyr::if_else(flag_type == "ok", 0.6, 0.9)),
          rad      = dplyr::if_else(is_sel, 7L,
                       dplyr::if_else(flag_type == "ok", 2L, 6L))
        )
      proxy %>%
        addCircleMarkers(
          data        = draw,
          lng         = ~lon_wgs,
          lat         = ~lat_wgs,
          layerId     = ~point_id,
          radius      = ~rad,
          color       = ~brd_col,
          fillColor   = ~fill_col,
          fillOpacity = ~opac,
          stroke      = TRUE,
          weight      = 1,
          popup       = ~paste0(
            "<b>", name, "</b><br>",
            format(timestamp_corrected, "%Y-%m-%d %H:%M"), "<br>",
            "Flag: ", flag_type, "<br>",
            "Speed: ", dplyr::if_else(is.na(step_speed_kmh), "—",
                                      paste0(round(step_speed_kmh, 1), " km/h"))
          )
        )
    }

    .draw_track <- function(proxy, d_pts) {
      if (nrow(d_pts) >= 2) {
        track_line <- sf::st_sfc(
          sf::st_linestring(as.matrix(d_pts[, c("lon_wgs", "lat_wgs")])),
          crs = 4326
        )
        proxy <- proxy %>%
          addPolylines(data = track_line, color = "#ffffff", weight = 1.5, opacity = 0.8)
      }
      proxy
    }

    # ── Base map ───────────────────────────────────────────────────────────
    output$map <- renderLeaflet({
      req(rv$data_flagged)
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addTiles(
          "https://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
          options = tileOptions(opacity = 0.9)
        ) %>%
        addScaleBar() %>%
        addDrawToolbar(
          rectangleOptions    = drawRectangleOptions(repeatMode = FALSE),
          polylineOptions     = FALSE,
          polygonOptions      = FALSE,
          circleOptions       = FALSE,
          markerOptions       = FALSE,
          circleMarkerOptions = FALSE,
          editOptions         = editToolbarOptions(edit = FALSE, remove = TRUE)
        ) %>%
        addLegend(
          position = "bottomleft",
          colors   = c(unname(FLAG_COLOURS[c("ok", "suspicious_vehicle", "suspicious_airborne",
                                             "predeployment", "immobility", "shift_episode")]),
                       MANUAL_SEL_COLOUR),
          labels   = c("Clean", "Possible vehicle", "Possible airborne",
                       "Pre-deployment", "Immobility", "Shift episode",
                       "Selected (manual)"),
          opacity  = 0.9,
          title    = "Flag type"
        )
    })

    # Full re-render with fitBounds — fires when the individual changes.
    # Pre-populate the manual selection with any removals already staged for
    # this individual so returning shows (and can re-adjust) them.
    observeEvent(input$individual, {
      req(nzchar(input$individual %||% ""))
      sel(manual_ids_for(input$individual))

      d_pts <- d_pts_data()
      req(nrow(d_pts) > 0)

      # Zoom to 5th–95th percentile so outlier flags don't stretch the view.
      lon_q <- stats::quantile(d_pts$lon_wgs, c(0.05, 0.95), na.rm = TRUE)
      lat_q <- stats::quantile(d_pts$lat_wgs, c(0.05, 0.95), na.rm = TRUE)

      proxy <- leafletProxy(session$ns("map"), session) %>%
        clearShapes() %>%
        clearMarkers()
      proxy <- .draw_track(proxy, d_pts)
      .draw_points(proxy, d_pts, sel()) %>%
        fitBounds(
          lng1 = lon_q[[1]] - 0.05, lat1 = lat_q[[1]] - 0.05,
          lng2 = lon_q[[2]] + 0.05, lat2 = lat_q[[2]] + 0.05
        )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Light redraw (no zoom reset) when the manual selection changes.
    observeEvent(sel(), {
      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts), nrow(d_pts) > 0)
      proxy <- leafletProxy(session$ns("map"), session) %>%
        clearShapes() %>%
        clearMarkers()
      proxy <- .draw_track(proxy, d_pts)
      .draw_points(proxy, d_pts, sel())
    }, ignoreInit = TRUE)

    # ── Click to select (+ all points within 10 m) ────────────────────────
    observeEvent(input$map_marker_click, {
      clicked <- input$map_marker_click$id
      req(!is.null(clicked), nzchar(clicked))
      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts))
      hit <- d_pts[d_pts$point_id == clicked, ]
      if (nrow(hit) == 0) return()

      utm_crs <- sf::st_crs(rv$data_flagged)
      all_utm <- sf::st_as_sf(d_pts, coords = c("lon_wgs", "lat_wgs"), crs = 4326) %>%
        sf::st_transform(utm_crs)
      hit_utm <- sf::st_as_sf(hit,   coords = c("lon_wgs", "lat_wgs"), crs = 4326) %>%
        sf::st_transform(utm_crs)
      buf     <- sf::st_buffer(hit_utm, dist = 10)
      nearby  <- d_pts$point_id[lengths(sf::st_within(all_utm, buf)) > 0]

      current <- sel()
      if (clicked %in% current) sel(setdiff(current, nearby))
      else                      sel(union(current, nearby))
    })

    # ── Box selection ──────────────────────────────────────────────────────
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      req(!is.null(feature))
      raw_coords <- feature$geometry$coordinates[[1]]
      coord_mat  <- do.call(rbind, lapply(raw_coords, function(c) c(c[[1]], c[[2]])))
      lng_min <- min(coord_mat[, 1]); lng_max <- max(coord_mat[, 1])
      lat_min <- min(coord_mat[, 2]); lat_max <- max(coord_mat[, 2])

      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts))
      in_box <- d_pts$point_id[
        d_pts$lon_wgs >= lng_min & d_pts$lon_wgs <= lng_max &
        d_pts$lat_wgs >= lat_min & d_pts$lat_wgs <= lat_max
      ]
      sel(union(sel(), in_box))
    })

    # ── Select all points before / after the current selection ────────────
    observeEvent(input$select_before, {
      req(length(sel()) > 0)
      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts))
      cutoff <- min(d_pts$timestamp_corrected[d_pts$point_id %in% sel()])
      before <- d_pts$point_id[d_pts$timestamp_corrected <= cutoff]
      sel(union(sel(), before))
    })

    observeEvent(input$select_after, {
      req(length(sel()) > 0)
      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts))
      cutoff <- max(d_pts$timestamp_corrected[d_pts$point_id %in% sel()])
      after  <- d_pts$point_id[d_pts$timestamp_corrected >= cutoff]
      sel(union(sel(), after))
    })

    observeEvent(input$clear_selected, { sel(character(0)) })

    output$selected_info <- renderUI({
      n <- length(sel())
      if (n == 0) return(tags$p(class = "text-muted", style = "font-size:0.82rem; margin:0.2rem 0",
                                 "No fixes staged for manual removal."))
      tags$p(style = paste0("font-size:0.82rem; margin:0.2rem 0; font-weight:600; color:",
                            MANUAL_SEL_COLOUR),
             n, " fix(es) staged for manual removal")
    })

    # ── Confirm: commit flag-type removals + staged manual points ──────────
    observeEvent(input$confirm, {
      req(nzchar(input$individual %||% ""))
      d <- ind_data()

      flag_types_to_remove <- character(0)
      if (input$rm_vehicle)  flag_types_to_remove <- c(flag_types_to_remove, "suspicious_vehicle")
      if (input$rm_airborne) flag_types_to_remove <- c(flag_types_to_remove, "suspicious_airborne")
      if (input$rm_predep)   flag_types_to_remove <- c(flag_types_to_remove, "predeployment")
      if (input$rm_immobile) flag_types_to_remove <- c(flag_types_to_remove, "immobility")
      if (input$rm_shift)    flag_types_to_remove <- c(flag_types_to_remove, "shift_episode")

      new_flag <- d %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(flag_type %in% flag_types_to_remove) %>%
        dplyr::select(name, timestamp_corrected, lon, lat, flag_type)

      staged <- sel()
      new_manual <- d %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(point_id = paste0(name, "|",
                                        format(timestamp_corrected, "%Y-%m-%d %H:%M:%S"))) %>%
        dplyr::filter(point_id %in% staged) %>%
        dplyr::select(name, timestamp_corrected, lon, lat) %>%
        dplyr::mutate(flag_type = "manual")

      # Commit flag removals (append, de-duplicated)
      if (nrow(new_flag) > 0) {
        existing <- rv$flagged_removals
        rv$flagged_removals <- if (is.null(existing)) new_flag else
          dplyr::bind_rows(existing, new_flag) %>%
          dplyr::distinct(name, timestamp_corrected, .keep_all = TRUE)
      }

      # Commit manual removals — staged selection is authoritative for this
      # individual: drop its previous manual rows, then add the current staged set.
      mr <- rv$manual_removals
      if (!is.null(mr)) mr <- dplyr::filter(mr, name != input$individual)
      if (nrow(new_manual) > 0)
        mr <- if (is.null(mr) || nrow(mr) == 0) new_manual else dplyr::bind_rows(mr, new_manual)
      if (!is.null(mr) && nrow(mr) == 0) mr <- NULL
      rv$manual_removals <- mr

      showNotification(
        paste0("Confirmed ", nrow(new_flag), " flag + ", nrow(new_manual),
               " manual removal(s) for ", input$individual, "."),
        type = "message"
      )

      inds <- flagged_inds()
      cur  <- which(inds == input$individual)
      if (length(cur) > 0 && cur < length(inds))
        updateSelectInput(session, "individual", selected = inds[cur + 1])
    })

    # ── Undo removals (flag + manual) for current individual ───────────────
    observeEvent(input$undo, {
      req(nzchar(input$individual %||% ""))
      fr <- rv$flagged_removals
      mr <- rv$manual_removals
      n_fr <- if (!is.null(fr)) sum(fr$name == input$individual) else 0
      n_mr <- if (!is.null(mr)) sum(mr$name == input$individual) else 0
      if (n_fr + n_mr == 0) {
        showNotification("No confirmed removals found for this individual.", type = "warning")
        return()
      }
      if (!is.null(fr)) {
        rv$flagged_removals <- dplyr::filter(fr, name != input$individual)
        if (nrow(rv$flagged_removals) == 0) rv$flagged_removals <- NULL
      }
      if (!is.null(mr)) {
        rv$manual_removals <- dplyr::filter(mr, name != input$individual)
        if (nrow(rv$manual_removals) == 0) rv$manual_removals <- NULL
      }
      sel(character(0))
      showNotification(
        paste0("Undone: ", n_fr, " flag + ", n_mr, " manual removal(s) cleared for ",
               input$individual, "."),
        type = "message"
      )
    })

    # ── Remove entire individual (all fixes → flagged removals) ────────────
    observeEvent(input$remove_individual, {
      req(nzchar(input$individual %||% ""))
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
      fr <- rv$flagged_removals
      mr <- rv$manual_removals
      reviewed_inds <- dplyr::n_distinct(c(
        if (!is.null(fr)) fr$name else character(0),
        if (!is.null(mr)) mr$name else character(0)
      ))
      total_removed <- (if (!is.null(fr)) nrow(fr) else 0) +
                       (if (!is.null(mr)) nrow(mr) else 0)
      tags$p(class = "text-muted", style = "font-size:0.85rem",
             reviewed_inds, " / ", total_inds, " individuals reviewed",
             tags$br(),
             total_removed, " fix(es) queued for removal (flag + manual)")
    })

    # ── Complete review ────────────────────────────────────────────────────
    observeEvent(input$complete, {
      fr <- rv$flagged_removals; mr <- rv$manual_removals
      total_removed <- (if (!is.null(fr)) nrow(fr) else 0) +
                       (if (!is.null(mr)) nrow(mr) else 0)
      showNotification(
        paste0("Review complete. ", total_removed, " fix(es) saved for removal."),
        type = "message", duration = 5
      )
      nav_select("main_nav", "Step 5: Manual Cleaning", session = parent_session)
    })

  })
}
