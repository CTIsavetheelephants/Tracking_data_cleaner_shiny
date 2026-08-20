mod_flag_ui <- function(id) {
  ns <- NS(id)

  # ── JS: prevent page-scroll jump when Leaflet draw events fire ──────────────
  # ── JS: clear drawn rectangles from the Leaflet draw toolbar ────────────────
  tagList(
  tags$script(HTML("
    /* Prevent scroll-to-top when a bbox rectangle is finished */
    $(document).on('draw:created draw:edited draw:deleted', function() {
      var pos = window.scrollY || window.pageYOffset;
      setTimeout(function() { window.scrollTo(0, pos); }, 50);
    });
    /* Remove all L.Rectangle layers from the draw toolbar feature group */
    Shiny.addCustomMessageHandler('leafletClearDrawnRects', function(data) {
      setTimeout(function() {
        var el = document.getElementById(data.mapId);
        if (!el || !el._leaflet_map) return;
        var map = el._leaflet_map;
        map.eachLayer(function(layer) {
          if (layer instanceof L.FeatureGroup) {
            var toRemove = [];
            layer.eachLayer(function(l) {
              if (l instanceof L.Rectangle) toRemove.push(l);
            });
            toRemove.forEach(function(l) { layer.removeLayer(l); });
          }
        });
      }, 100);
    });
  ")),

  layout_columns(
    col_widths = c(4, 8),

    card(
      card_header("Flagging Parameters"),
      h6("Draw Headquarter or Airstrip Buffer as Spatial Filter"),
      helpText("Click 'Place HQ / airstrip marker' then click the map to flag points",
               "within the buffer radius below — used to exclude fixes clustered at a",
               "research base or airstrip."),
      actionButton(ns("toggle_hq_mode"), "Place HQ / airstrip marker",
                   class = "btn-outline-secondary w-100"),
      sliderInput(ns("hq_radius_m"), "Buffer radius (m)",
                  min = 50, max = 5000, value = 500, step = 50),
      helpText("Points within this distance of the marker will be flagged as 'hq'."),
      actionButton(ns("clear_hq"),   "Clear HQ / airstrip marker",
                   class = "btn-outline-secondary w-100 mt-1"),
      actionButton(ns("clear_bbox"), "Clear bounding box",
                   class = "btn-outline-secondary w-100 mt-1"),
      hr(),
      h6("Speed thresholds"),
      sliderInput(ns("speed_vehicle"), "Possible vehicle (km/h)",
                  min = 5, max = 100, value = 25, step = 1),
      helpText("Steps faster than this are flagged as possible vehicle transport.",
               "Elephants rarely exceed 15 km/h; values above ~25 km/h suggest a vehicle."),
      sliderInput(ns("speed_air"), "Possible airborne (km/h)",
                  min = 50, max = 500, value = 150, step = 10),
      helpText("Steps faster than this are flagged as possible airborne transport",
               "(e.g. collar shipped by plane or helicopter)."),
      hr(),
      h6("Pre-deployment detection"),
      sliderInput(ns("max_ele_speed"), "Max elephant speed (km/h)",
                  min = 1, max = 30, value = 15, step = 1),
      helpText("The fastest a real elephant fix-to-fix step is expected to be.",
               "Used to identify where genuine tracking begins."),
      sliderInput(ns("min_valid_fixes"), "Min consecutive clean fixes",
                  min = 3, max = 20, value = 5, step = 1),
      helpText("How many consecutive fixes — all below the max elephant speed —",
               "must appear before the algorithm considers the collar properly deployed.",
               "Fixes before that window are marked as pre-deployment."),
      sliderInput(ns("max_predep_days"), "Max pre-deployment window (days)",
                  min = 7, max = 365, value = 30, step = 1),
      helpText("Pre-deployment fixes are only searched for within this many calendar",
               "days of each individual's first fix. Using a fixed time window rather",
               "than a fraction of the track prevents high-fix-rate collars (SKY) from",
               "having months of genuine field data swept up as pre-deployment."),
      helpText("Within this same early window, the algorithm also checks for a",
               "stationary cluster (collar sitting in camp before fitting).",
               "A cluster qualifies if all fixes stay within the Cluster radius",
               "and the stay spans at least the Minimum duration — both set in the",
               "Immobility detection section below. Fixes up to and including the",
               "end of that cluster are flagged as pre-deployment, not immobility."),
      hr(),
      h6("Immobility detection"),
      sliderInput(ns("immob_radius_m"), "Cluster radius (m)",
                  min = 10, max = 500, value = 200, step = 10),
      helpText("An individual is considered immobile if all fixes remain within this",
               "radius of the starting point of the cluster."),
      sliderInput(ns("immob_days"), "Minimum duration (days)",
                  min = 1, max = 30, value = 5, step = 1),
      helpText("Flag the cluster only if it spans at least this many days.",
               "May indicate collar drop, death, or extended resting."),
      sliderInput(ns("immob_min_fixes"), "Minimum fixes in cluster",
                  min = 2, max = 50, value = 10, step = 1),
      helpText("Flag the cluster only if it contains at least this many fixes.",
               "Prevents a data gap between two coincidentally co-located fixes",
               "from being mistaken for a genuine immobility episode."),
      sliderInput(ns("immob_end_frac"), "Post-deployment end fraction (%)",
                  min = 0.5, max = 30, value = 5, step = 0.5),
      helpText("Only flag an immobility cluster if it ends with fewer than this",
               "percentage of fixes remaining in the track. A cluster in the last",
               "5% of fixes may indicate a death or collar removal and is flagged",
               "(along with all subsequent points). A cluster with more than this",
               "fraction of the track still to come is treated as a genuine rest",
               "and left completely unflagged."),
      hr(),
      h6("Erroneous shift detection"),
      numericInput(ns("shift_m"), "Shift distance threshold (m)",
                   value = 10000, min = 1000, step = 500),
      helpText("A fix is flagged if it is this far from both its previous and next fix.",
               "i.e. the animal jumped far away and immediately came back."),
      numericInput(ns("max_gap_hours"), "Maximum time gap to neighbours (hours)",
                   value = 48, min = 0.1, step = 0.1),
      helpText("Only flag a fix if the time to both its neighbours is shorter than this.",
               "If either gap is larger, the distance may reflect real movement during a",
               "data gap and the fix is left unflagged."),
      hr(),
      actionButton(ns("run"),   "Run flagging",   class = "btn-primary w-100"),
      actionButton(ns("clear"), "Clear flagging", class = "btn-outline-danger w-100 mt-2"),
      hr(),
      actionButton(ns("complete"), "Complete → Step 4: Flag Review", class = "btn-outline-primary w-100")
    ),

    tagList(
      card(
        card_header("Draw Bounding Box as Spatial Filter"),
        p(class = "text-muted", style = "font-size:0.82rem; margin-bottom:0.4rem",
          "To draw a bounding box: click the", tags$b("rectangle icon"), "in the top-left toolbar on the map, then drag to draw.",
          "Use the trash icon in the same toolbar to remove a drawn rectangle.",
          "To place HQ: click 'Place HQ / airstrip marker' on the left, then click the map."),
        radioButtons(ns("map_plot_mode"), NULL, inline = TRUE,
                     choices  = c("Plot a sample (faster)" = "sample",
                                  "Plot all points" = "all"),
                     selected = "sample"),
        helpText(style = "font-size:0.78rem; margin-top:-0.35rem;",
                 "A sample plots quickly and keeps the map responsive on large",
                 "datasets (the default). Switch to", tags$b("Plot all points"),
                 "to see every outlier before finalising a tight box — this can be",
                 "slow to load and pan on very large datasets (100k+ fixes)."),
        leafletOutput(ns("spatial_map"), height = "380px"),
        uiOutput(ns("spatial_status"))
      ),
      card(
        card_header("Flag Summary"),
        uiOutput(ns("flag_counts")),
        hr(),
        DTOutput(ns("flag_table")),
        hr(),
        h6("Per-individual breakdown"),
        DTOutput(ns("ind_flag_tbl"))
      )
    )
  ) # close layout_columns
  ) # close tagList
}

mod_flag_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    hq_mode <- reactiveVal(FALSE)

    # ── Default max_gap_hours from data fix interval ───────────────────────
    observe({
      if (is.null(rv$median_fix_interval)) {
        updateNumericInput(session, "max_gap_hours", value = 48)
      } else {
        updateNumericInput(session, "max_gap_hours", value = rv$median_fix_interval)
      }
    })

    # ── Pre-populate HQ radius from site config if loaded ─────────────────
    observe({
      req(!is.null(rv$hq_point), !is.null(rv$hq_point$radius_m))
      updateSliderInput(session, "hq_radius_m", value = rv$hq_point$radius_m)
    })

    # ── Spatial filter map ─────────────────────────────────────────────────
    # Sample size used when "Plot a sample (faster)" is selected.
    .map_sample_n <- 10000L

    # All finite WGS84 coordinates of the current data (matrix of lon/lat).
    .map_all_coords <- function() {
      cc <- sf::st_coordinates(sf::st_transform(rv$data_raw, 4326))
      cc[is.finite(cc[, 1]) & is.finite(cc[, 2]), , drop = FALSE]
    }
    # Coordinates to plot for the chosen mode (systematic sample keeps the
    # spatial spread representative rather than clustering on one animal/period).
    .map_plot_coords <- function(coords, mode) {
      n_all <- nrow(coords)
      if (identical(mode, "sample") && n_all > .map_sample_n)
        coords <- coords[round(seq(1, n_all, length.out = .map_sample_n)), , drop = FALSE]
      coords
    }

    # Base map + initial points.  Zoom to the full extent (all points) so every
    # outlier is visible for drawing an accurate box.  The toggle observer below
    # re-plots points via a proxy so flipping all/sample never rebuilds the map
    # (which would wipe a drawn rectangle or HQ marker).
    output$spatial_map <- renderLeaflet({
      req(rv$data_raw)
      coords  <- .map_all_coords()
      lon_rng <- range(coords[, 1], na.rm = TRUE)
      lat_rng <- range(coords[, 2], na.rm = TRUE)
      pts     <- .map_plot_coords(coords, isolate(input$map_plot_mode) %||% "sample")

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
        addCircleMarkers(
          lng = pts[, 1], lat = pts[, 2], radius = 3,
          color = "#3388ff", fillColor = "#3388ff",
          fillOpacity = 0.6, stroke = FALSE, group = "data_pts"
        ) %>%
        fitBounds(
          lng1 = lon_rng[[1]], lat1 = lat_rng[[1]],
          lng2 = lon_rng[[2]], lat2 = lat_rng[[2]]
        )
    })

    # Re-plot points when the all/sample toggle flips (map already exists).
    observeEvent(input$map_plot_mode, {
      req(rv$data_raw)
      pts <- .map_plot_coords(.map_all_coords(), input$map_plot_mode %||% "sample")
      leafletProxy(session$ns("spatial_map"), session) %>%
        clearGroup("data_pts") %>%
        addCircleMarkers(
          lng = pts[, 1], lat = pts[, 2], radius = 3,
          color = "#3388ff", fillColor = "#3388ff",
          fillOpacity = 0.6, stroke = FALSE, group = "data_pts"
        )
    }, ignoreInit = TRUE)

    # Redraw HQ marker + buffer whenever rv$hq_point or hq_radius_m changes
    observe({
      req(rv$data_raw)
      proxy <- leafletProxy(session$ns("spatial_map"), session) %>%
        clearGroup("hq")
      if (!is.null(rv$hq_point)) {
        proxy %>%
          addCircleMarkers(
            lng         = rv$hq_point$lon,
            lat         = rv$hq_point$lat,
            radius      = 8,
            color       = FLAG_COLOURS[["hq"]],
            fillColor   = FLAG_COLOURS[["hq"]],
            fillOpacity = 0.9,
            stroke      = TRUE,
            weight      = 2,
            group       = "hq",
            popup       = paste0("HQ: ", round(rv$hq_point$lat, 5), "°, ",
                                 round(rv$hq_point$lon, 5), "°")
          ) %>%
          addCircles(
            lng         = rv$hq_point$lon,
            lat         = rv$hq_point$lat,
            radius      = input$hq_radius_m,
            color       = FLAG_COLOURS[["hq"]],
            fillColor   = FLAG_COLOURS[["hq"]],
            fillOpacity = 0.12,
            stroke      = TRUE,
            weight      = 1.5,
            group       = "hq"
          )
      }
    })

    # Redraw bbox rectangle whenever rv$bbox changes
    observe({
      req(rv$data_raw)
      proxy <- leafletProxy(session$ns("spatial_map"), session) %>%
        clearGroup("bbox")
      if (!is.null(rv$bbox)) {
        proxy %>% addRectangles(
          lng1        = rv$bbox$lon_min, lat1 = rv$bbox$lat_min,
          lng2        = rv$bbox$lon_max, lat2 = rv$bbox$lat_max,
          color       = FLAG_COLOURS[["outside_bbox"]],
          fillColor   = FLAG_COLOURS[["outside_bbox"]],
          fillOpacity = 0.08,
          weight      = 2,
          group       = "bbox"
        )
      }
    })

    # Capture rectangle drawn on map → set rv$bbox
    observeEvent(input$spatial_map_draw_new_feature, {
      feat      <- input$spatial_map_draw_new_feature
      raw_coords <- feat$geometry$coordinates[[1]]
      coord_mat  <- do.call(rbind, lapply(raw_coords, function(c) c(c[[1]], c[[2]])))
      rv$bbox <- list(
        lon_min = min(coord_mat[, 1]), lon_max = max(coord_mat[, 1]),
        lat_min = min(coord_mat[, 2]), lat_max = max(coord_mat[, 2])
      )
      showNotification("Bounding box set.", type = "message")
    })

    # Toolbar trash icon → clear rv$bbox
    observeEvent(input$spatial_map_draw_deleted_features, {
      rv$bbox <- NULL
    })

    # HQ mode toggle
    observeEvent(input$toggle_hq_mode, {
      hq_mode(!hq_mode())
      updateActionButton(session, "toggle_hq_mode",
                         label = if (hq_mode()) "Click map to place marker ✓"
                                 else "Place HQ / airstrip marker")
    })

    # Map click → set HQ when in HQ mode
    observeEvent(input$spatial_map_click, {
      req(hq_mode())
      click <- input$spatial_map_click
      rv$hq_point <- list(lon = click$lng, lat = click$lat,
                          radius_m = input$hq_radius_m)
      hq_mode(FALSE)
      updateActionButton(session, "toggle_hq_mode", label = "Place HQ / airstrip marker")
      showNotification("HQ / airstrip marker placed.", type = "message")
    })

    observeEvent(input$clear_hq, {
      rv$hq_point <- NULL
      showNotification("HQ marker cleared.", type = "warning")
    })

    observeEvent(input$clear_bbox, {
      rv$bbox <- NULL
      # Also remove the rectangle that the draw toolbar drew on the map
      session$sendCustomMessage("leafletClearDrawnRects",
                                list(mapId = session$ns("spatial_map")))
      showNotification("Bounding box cleared. Draw a new rectangle on the map to set one.", type = "warning")
    })

    output$spatial_status <- renderUI({
      hq_txt   <- if (!is.null(rv$hq_point))
        paste0("HQ: ", round(rv$hq_point$lat, 4), "°, ", round(rv$hq_point$lon, 4),
               "° (±", input$hq_radius_m, " m)")
      else "HQ: not set"
      bbox_txt <- if (!is.null(rv$bbox))
        paste0("Bbox: lon ", round(rv$bbox$lon_min, 3), " – ", round(rv$bbox$lon_max, 3),
               ", lat ", round(rv$bbox$lat_min, 3), " – ", round(rv$bbox$lat_max, 3))
      else "Bbox: not set"
      n_all    <- if (!is.null(rv$data_raw)) nrow(rv$data_raw) else 0L
      pts_txt  <- if (identical(input$map_plot_mode %||% "sample", "sample") &&
                      n_all > .map_sample_n)
        paste0("Showing a sample of ", format(.map_sample_n, big.mark = ","),
               " of ", format(n_all, big.mark = ","), " fixes")
      else
        paste0("Showing all ", format(n_all, big.mark = ","), " fixes")
      tags$p(class = "text-muted", style = "font-size:0.8rem; margin:0.4rem 0 0",
             pts_txt, tags$br(), hq_txt, tags$br(), bbox_txt)
    })

    # ── Run flagging ───────────────────────────────────────────────────────
    observeEvent(input$run, {
      req(rv$data_raw)

      withProgress(message = "Running flagging pipeline...", {

        dat <- rv$data_raw %>%
          dplyr::select(name, timestamp_corrected, x, y, geometry)
        gc()

        # ── Step 1: Spatial filters first ─────────────────────────────────
        # Bbox and HQ are applied before speed / predeployment / immobility so
        # that out-of-area GPS fixes (erroneous locations, pre-shipment fixes,
        # fixes near the base camp) cannot create false high-speed steps between
        # a valid in-area fix and a distant out-of-area fix.
        setProgress(0.05, detail = "Spatial filters")
        if (!is.null(rv$hq_point)) {
          dat <- flag_hq(dat, rv$hq_point$lon, rv$hq_point$lat, input$hq_radius_m)
          rv$hq_point$radius_m <- input$hq_radius_m
        } else {
          dat$hq_flag <- FALSE
        }
        if (!is.null(rv$bbox)) {
          dat <- flag_outside_bbox(dat,
                                   rv$bbox$lon_min, rv$bbox$lon_max,
                                   rv$bbox$lat_min, rv$bbox$lat_max)
        } else {
          dat$outside_bbox <- FALSE
        }
        gc()

        # Split into spatially-valid and spatially-excluded subsets.
        # Speed, pre-deployment, shift episode, and immobility algorithms
        # only run on the valid subset so excluded fixes don't distort results.
        dat_out <- dat %>%
          dplyr::filter(outside_bbox | hq_flag) %>%
          dplyr::mutate(
            step_m          = NA_real_,
            step_s          = NA_real_,
            step_speed_kmh  = NA_real_,
            speed_flag      = "first_fix",
            predeployment_auto = FALSE,
            shift_episode   = FALSE,
            immobility      = FALSE
          )
        dat <- dat %>% dplyr::filter(!outside_bbox, !hq_flag)
        gc()

        # ── Step 2: Behavioural flags on spatially-valid fixes ─────────────
        setProgress(0.2, detail = "Speed flags")
        dat <- flag_by_speed(dat,
                             speed_vehicle_kmh = input$speed_vehicle,
                             speed_air_kmh     = input$speed_air)
        gc()

        setProgress(0.4, detail = "Pre-deployment detection")
        dat <- detect_predeployment(dat,
                                    max_elephant_speed_kmh = input$max_ele_speed,
                                    min_valid_fixes        = input$min_valid_fixes,
                                    max_predep_days        = input$max_predep_days,
                                    immob_radius_m         = input$immob_radius_m,
                                    immob_min_days         = input$immob_days)
        gc()

        setProgress(0.58, detail = "Shift episode detection")
        shift_result <- detect_shift_episodes(dat,
                                              shift_m       = input$shift_m,
                                              max_gap_hours = input$max_gap_hours)
        episodes <- shift_result$episodes
        dat <- mark_shift_episodes(shift_result$with_steps, episodes)
        rm(shift_result); gc()

        setProgress(0.74, detail = "Immobility detection")
        dat <- flag_immobility(dat,
                               radius_m     = input$immob_radius_m,
                               min_days     = input$immob_days,
                               min_fixes    = input$immob_min_fixes,
                               end_fraction = input$immob_end_frac / 100)
        gc()

        # ── Step 3: Recombine valid + excluded fixes ───────────────────────
        dat <- dplyr::bind_rows(dat, dat_out) %>%
          dplyr::arrange(name, timestamp_corrected)
        rm(dat_out); gc()

        setProgress(0.88, detail = "Assigning colours")
        dat <- assign_flag_colour(dat)
        gc()

        flag_cols <- c("name", "timestamp_corrected", "step_m", "step_s",
                       "step_speed_kmh", "speed_flag", "predeployment_auto",
                       "shift_episode", "immobility", "hq_flag", "outside_bbox",
                       "point_color", "flag_type")
        flag_tbl <- dat %>%
          sf::st_drop_geometry() %>%
          dplyr::select(dplyr::any_of(flag_cols))
        rm(dat); gc()

        flag_join_cols <- setdiff(flag_cols, c("name", "timestamp_corrected"))
        out <- rv$data_raw %>%
          dplyr::select(-dplyr::any_of(flag_join_cols)) %>%
          dplyr::left_join(flag_tbl, by = c("name", "timestamp_corrected"))
        rm(flag_tbl); gc()

        coords_wgs  <- sf::st_coordinates(sf::st_transform(out, 4326))
        out$lon_wgs <- coords_wgs[, 1]
        out$lat_wgs <- coords_wgs[, 2]

        rv$shift_episodes <- episodes

        # Auto-remove spatial flags — logged for export, stripped from review
        auto_rm <- out %>%
          sf::st_drop_geometry() %>%
          dplyr::filter(flag_type %in% c("outside_bbox", "hq")) %>%
          dplyr::select(name, timestamp_corrected, lon, lat, flag_type)
        message("Auto-remove spatial: ", nrow(auto_rm), " fix(es) (",
                sum(auto_rm$flag_type == "hq"), " hq, ",
                sum(auto_rm$flag_type == "outside_bbox"), " outside_bbox)")
        if (nrow(auto_rm) > 0) {
          existing <- rv$flagged_removals
          rv$flagged_removals <- if (is.null(existing)) auto_rm else {
            dplyr::bind_rows(existing, auto_rm) %>%
              dplyr::distinct(name, timestamp_corrected, .keep_all = TRUE)
          }
          out <- out %>% dplyr::filter(!flag_type %in% c("outside_bbox", "hq"))
        }
        message("data_flagged rows after spatial auto-remove: ", nrow(out),
                " (hq_flag still TRUE in flagged: ",
                sum(out$hq_flag, na.rm = TRUE), ")")
        rv$data_flagged <- out

        showNotification("Flagging complete.", type = "message")
      })
    })

    # ── Flag summary ───────────────────────────────────────────────────────
    flag_summary <- reactive({
      req(rv$data_flagged)
      dat      <- rv$data_flagged
      auto_rm  <- rv$flagged_removals
      all_types <- c("ok", "suspicious_vehicle", "suspicious_airborne",
                     "predeployment", "immobility", "shift_episode",
                     "outside_bbox", "hq")
      # outside_bbox and hq were auto-removed from dat — read their counts from rv$flagged_removals
      spatial_types <- c("outside_bbox", "hq")
      tibble::tibble(
        Flag = all_types,
        Count = vapply(all_types, function(t) {
          n <- sum(dat$flag_type == t, na.rm = TRUE)
          if (t %in% spatial_types && !is.null(auto_rm))
            n <- n + sum(auto_rm$flag_type == t, na.rm = TRUE)
          n
        }, integer(1)),
        Individuals = vapply(all_types, function(t) {
          inds <- dat$name[dat$flag_type == t]
          if (t %in% spatial_types && !is.null(auto_rm))
            inds <- c(inds, auto_rm$name[auto_rm$flag_type == t])
          dplyr::n_distinct(inds)
        }, integer(1))
      )
    })

    output$flag_counts <- renderUI({
      req(rv$data_flagged)
      s <- flag_summary()
      total_flagged <- sum(s$Count[s$Flag != "ok"])
      tags$p(class = "text-muted mt-2",
             total_flagged, " flagged fixes out of ", nrow(rv$data_flagged), " total")
    })

    output$flag_table <- renderDT({
      lbl <- c(ok = "Clean",
               suspicious_vehicle  = "Possible vehicle",
               suspicious_airborne = "Possible airborne",
               predeployment       = "Pre-deployment",
               immobility          = "Immobility",
               shift_episode       = "Shift episode",
               outside_bbox        = "Outside bounding box",
               hq                  = "HQ / airstrip buffer")
      flag_summary() %>%
        dplyr::mutate(Flag = dplyr::coalesce(unname(lbl[Flag]), Flag))
    }, options = list(dom = "t", pageLength = 10), rownames = FALSE)

    output$ind_flag_tbl <- renderDT({
      build_ind_flag_breakdown(rv)
    }, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
       rownames = FALSE, class = "compact cell-border stripe")

    # ── Clear flagging ─────────────────────────────────────────────────────
    observeEvent(input$clear, {
      rv$data_flagged   <- NULL
      rv$shift_episodes <- NULL
      rv$flagged_removals <- NULL
      showNotification("Flagging cleared. Adjust thresholds and run again.", type = "warning")
    })

    observeEvent(input$complete, {
      req(rv$data_flagged)
      nav_select("main_nav", "Step 4: Flag Review", session = parent_session)
    })

  })
}
