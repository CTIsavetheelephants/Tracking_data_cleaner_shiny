mod_clean_ui <- function(id) {
  ns <- NS(id)
  tagList(
  layout_columns(
    col_widths = c(3, 9),

    card(
      card_header("Manual Exclusion"),
      selectInput(ns("individual"), "Select individual", choices = NULL),
      actionButton(ns("prev_ind"), icon("arrow-left"),  class = "btn-sm"),
      actionButton(ns("next_ind"), icon("arrow-right"), class = "btn-sm"),
      hr(),
      p(class = "text-muted", style = "font-size:0.85rem",
        "Click points on the map to select them for removal."),
      uiOutput(ns("selected_info")),
      actionButton(ns("add_selected"), "Add selected to removal list",
                   class = "btn-warning w-100 mt-2"),
      actionButton(ns("select_before"), "Select all points before selected",
                   class = "btn-outline-warning w-100 mt-1"),
      actionButton(ns("select_after"),  "Select all points after selected",
                   class = "btn-outline-warning w-100 mt-1"),
      actionButton(ns("clear_selected"), "Clear selection",
                   class = "btn-outline-secondary w-100 mt-1"),
      hr(),
      uiOutput(ns("removal_summary")),
      actionButton(ns("undo_last"), "Undo last addition",
                   class = "btn-outline-danger w-100 mt-1"),
      hr(),
      actionButton(ns("complete"), "Complete clean → Step 6: Export",
                   class = "btn-primary w-100")
    ),

    card(
      card_header("Individual track (post-flag removals)"),
      leafletOutput(ns("map"), height = "600px")
    )
  ),
  card(
    card_header("Removal summary by individual"),
    DTOutput(ns("ind_removal_tbl"))
  )
  ) # close tagList
}

mod_clean_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    sel <- reactiveVal(character(0))  # selected point IDs (name|timestamp)

    observeEvent(rv$data_raw, {
      if (is.null(rv$data_raw)) sel(character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ── Individual list ────────────────────────────────────────────────────
    observe({
      req(rv$data_flagged)
      inds <- sort(unique(rv$data_flagged$name))
      updateSelectInput(session, "individual",
                        choices  = c("— select an individual —" = "", inds),
                        selected = "")
    })

    observeEvent(input$prev_ind, {
      inds <- sort(unique(rv$data_flagged$name))
      cur  <- which(inds == input$individual)
      if (length(cur) == 0) {
        sel(character(0))
        updateSelectInput(session, "individual", selected = inds[1])
      } else if (cur > 1) {
        sel(character(0))
        updateSelectInput(session, "individual", selected = inds[cur - 1])
      }
    })

    observeEvent(input$next_ind, {
      inds <- sort(unique(rv$data_flagged$name))
      cur  <- which(inds == input$individual)
      if (length(cur) == 0) {
        sel(character(0))
        updateSelectInput(session, "individual", selected = inds[1])
      } else if (cur < length(inds)) {
        sel(character(0))
        updateSelectInput(session, "individual", selected = inds[cur + 1])
      }
    })

    # ── Current individual clean data ──────────────────────────────────────
    # Filter to individual first, then apply removals only for that individual —
    # avoids anti_join across the full dataset on every removal change.
    ind_data <- reactive({
      req(rv$data_flagged, input$individual)
      d <- rv$data_flagged %>%
        dplyr::filter(name == input$individual) %>%
        dplyr::arrange(timestamp_corrected)

      fr <- rv$flagged_removals
      if (!is.null(fr) && nrow(fr) > 0) {
        fr_ind <- dplyr::filter(fr, name == input$individual)
        if (nrow(fr_ind) > 0)
          d <- dplyr::anti_join(d, fr_ind, by = c("name", "timestamp_corrected"))
      }

      mr <- rv$manual_removals
      if (!is.null(mr) && nrow(mr) > 0) {
        mr_ind <- dplyr::filter(mr, name == input$individual)
        if (nrow(mr_ind) > 0)
          d <- dplyr::anti_join(d, mr_ind, by = c("name", "timestamp_corrected"))
      }

      d
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
        addDrawToolbar(
          rectangleOptions   = drawRectangleOptions(),
          polylineOptions    = FALSE,
          polygonOptions     = FALSE,
          circleOptions      = FALSE,
          markerOptions      = FALSE,
          circleMarkerOptions = FALSE,
          editOptions        = editToolbarOptions(edit = FALSE, remove = TRUE)
        )
    })

    # Shared helper: build d_pts from current individual
    d_pts_data <- reactive({
      d <- ind_data()
      req(nrow(d) > 0)
      d %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          point_id = paste0(name, "|", format(timestamp_corrected, "%Y-%m-%d %H:%M:%S"))
        ) %>%
        dplyr::select(name, timestamp_corrected, lon_wgs, lat_wgs, point_id)
    })

    # Full re-render with fitBounds — fires when individual changes
    observeEvent(input$individual, {
      d_pts <- d_pts_data()
      req(nrow(d_pts) > 0)

      d_pts <- d_pts %>%
        dplyr::mutate(fill_col = dplyr::if_else(point_id %in% sel(), "#ff0000", "#2166ac"))

      proxy <- leafletProxy(session$ns("map"), session) %>%
        clearShapes() %>%
        clearMarkers()

      # Draw HQ buffer for reference if one was set during flagging
      if (!is.null(rv$hq_point)) {
        proxy <- proxy %>%
          addCircles(
            lng         = rv$hq_point$lon,
            lat         = rv$hq_point$lat,
            radius      = rv$hq_point$radius_m,
            color       = "#e67e22",
            fillColor   = "#e67e22",
            fillOpacity = 0.08,
            stroke      = TRUE,
            weight      = 1.5,
            dashArray   = "6,4"
          )
      }

      if (nrow(d_pts) >= 2) {
        track_line <- sf::st_sfc(
          sf::st_linestring(as.matrix(d_pts[, c("lon_wgs", "lat_wgs")])),
          crs = 4326
        )
        proxy <- proxy %>%
          addPolylines(data = track_line, color = "#ffffff", weight = 1.5, opacity = 0.8)
      }

      proxy %>%
        addCircleMarkers(
          data        = d_pts,
          lng         = ~lon_wgs,
          lat         = ~lat_wgs,
          layerId     = ~point_id,
          radius      = 4,
          color       = "white",
          fillColor   = ~fill_col,
          fillOpacity = 1,
          stroke      = TRUE,
          weight      = 1,
          popup       = ~paste0("<b>", name, "</b><br>",
                                format(timestamp_corrected, "%Y-%m-%d %H:%M"))
        ) %>%
        fitBounds(
          lng1 = min(d_pts$lon_wgs) - 0.01, lat1 = min(d_pts$lat_wgs) - 0.01,
          lng2 = max(d_pts$lon_wgs) + 0.01, lat2 = max(d_pts$lat_wgs) + 0.01
        )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Redraw line + markers without resetting zoom — fires when selection or
    # removal list changes. clearShapes() is required so the polyline is
    # redrawn from the current (post-removal) data, keeping it in sync with
    # the markers.
    observeEvent(list(sel(), rv$manual_removals), {
      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts), nrow(d_pts) > 0)

      d_pts <- d_pts %>%
        dplyr::mutate(fill_col = dplyr::if_else(point_id %in% sel(), "#ff0000", "#2166ac"))

      proxy <- leafletProxy(session$ns("map"), session) %>%
        clearShapes() %>%
        clearMarkers()

      if (nrow(d_pts) >= 2) {
        track_line <- sf::st_sfc(
          sf::st_linestring(as.matrix(d_pts[, c("lon_wgs", "lat_wgs")])),
          crs = 4326
        )
        proxy <- proxy %>%
          addPolylines(data = track_line, color = "#ffffff", weight = 1.5, opacity = 0.8)
      }

      proxy %>%
        addCircleMarkers(
          data        = d_pts,
          lng         = ~lon_wgs,
          lat         = ~lat_wgs,
          layerId     = ~point_id,
          radius      = 4,
          color       = "white",
          fillColor   = ~fill_col,
          fillOpacity = 1,
          stroke      = TRUE,
          weight      = 1,
          popup       = ~paste0("<b>", name, "</b><br>",
                                format(timestamp_corrected, "%Y-%m-%d %H:%M"))
        )
    }, ignoreInit = TRUE)


    # ── Click to select (+ all points within 10 m) ────────────────────────
    observeEvent(input$map_marker_click, {
      clicked <- input$map_marker_click$id
      req(!is.null(clicked), nzchar(clicked))

      d_pts <- tryCatch(d_pts_data(), error = function(e) NULL)
      req(!is.null(d_pts))

      # Find the clicked point's location
      hit <- d_pts[d_pts$point_id == clicked, ]
      if (nrow(hit) == 0) return()

      # Build sf for all points in UTM (same CRS as rv$data_flagged)
      utm_crs  <- sf::st_crs(rv$data_flagged)
      all_utm  <- sf::st_as_sf(d_pts, coords = c("lon_wgs", "lat_wgs"), crs = 4326) %>%
        sf::st_transform(utm_crs)
      hit_utm  <- sf::st_as_sf(hit,   coords = c("lon_wgs", "lat_wgs"), crs = 4326) %>%
        sf::st_transform(utm_crs)

      buf      <- sf::st_buffer(hit_utm, dist = 10)
      nearby   <- d_pts$point_id[lengths(sf::st_within(all_utm, buf)) > 0]

      current <- sel()
      if (clicked %in% current) {
        sel(setdiff(current, nearby))
      } else {
        sel(union(current, nearby))
      }
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

    output$selected_info <- renderUI({
      n <- length(sel())
      if (n == 0) return(tags$p(class = "text-muted", style = "font-size:0.85rem",
                                 "No points selected."))
      tags$p(class = "text-warning", style = "font-size:0.85rem",
             n, " point(s) selected")
    })

    observeEvent(input$clear_selected, {
      sel(character(0))
    })

    # ── Select all points before / after the earliest / latest selected ────
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

    # ── Add selected to removal list ───────────────────────────────────────
    observeEvent(input$add_selected, {
      req(length(sel()) > 0)

      d <- ind_data() %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          point_id = paste0(name, "|", format(timestamp_corrected, "%Y-%m-%d %H:%M:%S"))
        ) %>%
        dplyr::filter(point_id %in% sel()) %>%
        dplyr::select(name, timestamp_corrected, lon, lat) %>%
        dplyr::mutate(flag_type = "manual")

      existing <- rv$manual_removals
      if (is.null(existing)) {
        rv$manual_removals <- d
      } else {
        rv$manual_removals <- dplyr::bind_rows(existing, d) %>%
          dplyr::distinct(name, timestamp_corrected, .keep_all = TRUE)
      }

      showNotification(paste0(nrow(d), " point(s) added to manual removal list."),
                       type = "message")
      sel(character(0))
    })

    # ── Undo last addition ─────────────────────────────────────────────────
    observeEvent(input$undo_last, {
      req(!is.null(rv$manual_removals), nrow(rv$manual_removals) > 0)
      rv$manual_removals <- rv$manual_removals[-nrow(rv$manual_removals), ]
      showNotification("Last manual removal undone.", type = "warning")
    })

    # ── Removal summary ────────────────────────────────────────────────────
    output$removal_summary <- renderUI({
      n <- if (!is.null(rv$manual_removals)) nrow(rv$manual_removals) else 0
      tags$p(class = "text-muted", style = "font-size:0.85rem",
             n, " manual removal(s) total")
    })

    output$ind_removal_tbl <- renderDT({
      req(rv$data_raw)
      build_individual_summary(rv)
    }, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
       rownames = FALSE, class = "compact cell-border stripe")

    # ── Complete clean ─────────────────────────────────────────────────────
    observeEvent(input$complete, {
      n <- if (!is.null(rv$manual_removals)) nrow(rv$manual_removals) else 0
      showNotification(
        paste0("Clean complete. ", n, " manual removal(s) saved."),
        type = "message", duration = 5
      )
      nav_select("main_nav", "Step 6: Export", session = parent_session)
    })

  })
}
