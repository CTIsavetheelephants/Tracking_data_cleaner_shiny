ASSIGN_PALETTE <- c(
  "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
  "#a65628", "#f781bf", "#1b9e77", "#d95f02", "#7570b3",
  "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854",
  "#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3"
)

mod_assign_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(3, 9),

    card(
      card_header("Region Assignment"),
      p(class = "text-muted", style = "font-size:0.85rem",
        "Draw a rectangle on the map to select individuals, or click points individually. Then assign them to a region."),
      uiOutput(ns("selected_info")),
      selectizeInput(ns("region"), "Assign selected to region", choices = NULL,
                     options = list(placeholder = "--- select or type ---", create = TRUE)),
      actionButton(ns("assign"),    "Assign selected",
                   class = "btn-success w-100 mt-1"),
      actionButton(ns("clear_sel"), "Clear selection",
                   class = "btn-outline-secondary w-100 mt-1"),
      hr(),
      h6("Bulk assign all unassigned"),
      selectizeInput(ns("bulk_region"), NULL, choices = NULL,
                     options = list(placeholder = "--- select or type ---", create = TRUE)),
      actionButton(ns("bulk_assign"), "Assign all unassigned to this region",
                   class = "btn-outline-secondary w-100 mt-1"),
      hr(),
      uiOutput(ns("progress")),
      hr(),
      actionButton(ns("complete"), "Complete → Step 3: Flag",
                   class = "btn-primary w-100"),
      actionButton(ns("skip"), "Skip → Step 3: Flag",
                   class = "btn-outline-secondary w-100 mt-1")
    ),

    card(
      card_header("Individual centroids — draw a box or click to select"),
      leafletOutput(ns("map"), height = "600px")
    )
  )
}

# Build legend HTML for addControl
.assign_legend_html <- function(pal, regs) {
  items <- paste(sapply(names(pal), function(nm) {
    reg       <- regs[nm]
    reg_label <- if (!is.na(reg) && nzchar(reg)) paste0(" <em style='color:#aaa'>→ ", reg, "</em>") else ""
    paste0(
      '<div style="display:flex;align-items:center;margin-bottom:2px;">',
      '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;',
      'background:', pal[nm], ';margin-right:5px;flex-shrink:0;"></span>',
      '<span style="font-size:11px;">', nm, reg_label, '</span>',
      '</div>'
    )
  }), collapse = "")
  paste0(
    '<div style="background:rgba(255,255,255,0.9);padding:8px 10px;border-radius:4px;',
    'max-height:300px;overflow-y:auto;box-shadow:0 1px 4px rgba(0,0,0,0.3);">',
    '<div style="font-size:11px;font-weight:600;margin-bottom:4px;">Individuals</div>',
    items,
    '</div>'
  )
}

mod_assign_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    sel <- reactiveVal(character(0))

    observeEvent(rv$data_raw, {
      if (is.null(rv$data_raw)) sel(character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ── Populate region dropdowns when data arrives ────────────────────────
    observe({
      req(rv$data_raw)
      updateSelectizeInput(session, "region",      choices = c("--- select ---" = ""))
      updateSelectizeInput(session, "bulk_region", choices = c("--- select ---" = ""))
    })

    # ── Individual colour palette (consistent sorted order) ────────────────
    ind_colors <- reactive({
      req(rv$data_raw)
      inds <- sort(unique(rv$data_raw$name))
      cols <- ASSIGN_PALETTE[((seq_along(inds) - 1L) %% length(ASSIGN_PALETTE)) + 1L]
      setNames(cols, inds)
    })

    # ── Centroids ──────────────────────────────────────────────────────────
    centroids <- reactive({
      req(rv$data_raw)
      rv$data_raw %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(lon = mean(lon, na.rm = TRUE),
                         lat = mean(lat, na.rm = TRUE),
                         .groups = "drop")
    })

    # ── Point styling ──────────────────────────────────────────────────────
    pt_colors <- reactive({
      cents  <- centroids()
      pal    <- ind_colors()
      regs   <- rv$individual_regions %||% character(0)
      is_sel <- cents$name %in% sel()

      assigned <- regs[cents$name]
      assigned[is.na(assigned)] <- ""

      cents %>%
        dplyr::mutate(
          region       = dplyr::if_else(nzchar(assigned), assigned, NA_character_),
          fill_color   = unname(pal[name]),
          border_color = dplyr::if_else(is_sel, "#ff9900", unname(pal[name])),
          radius       = dplyr::if_else(is_sel, 10L, 7L),
          weight       = dplyr::if_else(is_sel, 2L, 0L),
          fill_opacity = dplyr::if_else(nzchar(assigned), 0.4, 0.9)
        )
    })

    # ── Selected count ─────────────────────────────────────────────────────
    output$selected_info <- renderUI({
      n <- length(sel())
      if (n == 0)
        tags$p(class = "text-muted", style = "font-size:0.85rem", "No individuals selected.")
      else
        tags$p(class = "text-warning", style = "font-size:0.85rem",
               n, " individual(s) selected")
    })

    # ── Progress ───────────────────────────────────────────────────────────
    output$progress <- renderUI({
      req(rv$data_raw)
      inds  <- sort(unique(rv$data_raw$name))
      regs  <- rv$individual_regions %||% character(0)
      n_ass <- sum(inds %in% names(regs[nzchar(regs)]))
      tags$p(class = "text-muted", style = "font-size:0.85rem",
             n_ass, " / ", length(inds), " individuals assigned")
    })

    # ── Map — render with coloured centroids and legend ────────────────────
    output$map <- renderLeaflet({
      req(rv$data_raw)
      d   <- centroids()
      pal <- ind_colors()
      regs <- rv$individual_regions %||% character(0)

      d$fill_color <- unname(pal[d$name])

      m <- leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addTiles(
          "https://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}",
          options = tileOptions(opacity = 0.9)
        ) %>%
        addScaleBar() %>%
        addDrawToolbar(
          rectangleOptions    = drawRectangleOptions(),
          polylineOptions     = FALSE,
          polygonOptions      = FALSE,
          circleOptions       = FALSE,
          markerOptions       = FALSE,
          circleMarkerOptions = FALSE,
          editOptions         = editToolbarOptions(edit = FALSE, remove = TRUE)
        )

      if (nrow(d) > 0) {
        m <- m %>%
          addCircleMarkers(
            data        = d,
            lng         = ~lon,
            lat         = ~lat,
            layerId     = ~name,
            radius      = 7,
            fillColor   = ~fill_color,
            fillOpacity = 0.9,
            color       = ~fill_color,
            stroke      = TRUE,
            weight      = 0,
            popup       = ~paste0("<b>", name, "</b><br>Unassigned")
          ) %>%
          fitBounds(
            lng1 = min(d$lon) - 0.5, lat1 = min(d$lat) - 0.5,
            lng2 = max(d$lon) + 0.5, lat2 = max(d$lat) + 0.5
          ) %>%
          addControl(
            html     = .assign_legend_html(pal, regs),
            position = "bottomright",
            layerId  = "ind_legend"
          )
      }
      m
    })

    # ── Update markers and legend when selection or assignments change ──────
    observe({
      d    <- pt_colors()
      pal  <- ind_colors()
      regs <- rv$individual_regions %||% character(0)
      req(nrow(d) > 0)

      leafletProxy(session$ns("map"), session) %>%
        clearMarkers() %>%
        addCircleMarkers(
          data        = d,
          lng         = ~lon,
          lat         = ~lat,
          layerId     = ~name,
          radius      = ~radius,
          fillColor   = ~fill_color,
          fillOpacity = ~fill_opacity,
          color       = ~border_color,
          stroke      = TRUE,
          weight      = ~weight,
          popup       = ~paste0(
            "<b>", name, "</b><br>",
            dplyr::if_else(!is.na(region), paste0("Region: ", region), "Unassigned")
          )
        ) %>%
        removeControl("ind_legend") %>%
        addControl(
          html     = .assign_legend_html(pal, regs),
          position = "bottomright",
          layerId  = "ind_legend"
        )
    })

    # ── Click to toggle selection ──────────────────────────────────────────
    observeEvent(input$map_marker_click, {
      clicked <- input$map_marker_click$id
      req(!is.null(clicked), nzchar(clicked))
      current <- sel()
      if (clicked %in% current) sel(setdiff(current, clicked))
      else                      sel(union(current, clicked))
    })

    # ── Rectangle draw to select individuals within box ───────────────────
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      req(!is.null(feature))
      raw_coords <- feature$geometry$coordinates[[1]]
      coord_mat  <- do.call(rbind, lapply(raw_coords, function(c) c(c[[1]], c[[2]])))
      lng_min <- min(coord_mat[, 1]); lng_max <- max(coord_mat[, 1])
      lat_min <- min(coord_mat[, 2]); lat_max <- max(coord_mat[, 2])
      d      <- centroids()
      in_box <- d$name[d$lon >= lng_min & d$lon <= lng_max &
                       d$lat >= lat_min & d$lat <= lat_max]
      sel(union(sel(), in_box))
    })

    # ── Assign selected to region ──────────────────────────────────────────
    observeEvent(input$assign, {
      req(length(sel()) > 0, nzchar(input$region %||% ""))
      regs        <- rv$individual_regions %||% character(0)
      regs[sel()] <- input$region
      rv$individual_regions <- regs
      showNotification(
        paste0(length(sel()), " individual(s) assigned to ", input$region, "."),
        type = "message"
      )
      sel(character(0))
    })

    # ── Clear selection ────────────────────────────────────────────────────
    observeEvent(input$clear_sel, { sel(character(0)) })

    # ── Bulk assign all unassigned ─────────────────────────────────────────
    observeEvent(input$bulk_assign, {
      req(nzchar(input$bulk_region %||% ""))
      inds       <- sort(unique(rv$data_raw$name))
      regs       <- rv$individual_regions %||% character(0)
      assigned   <- names(regs[nzchar(regs)])
      unassigned <- setdiff(inds, assigned)
      if (length(unassigned) == 0) {
        showNotification("All individuals are already assigned.", type = "warning")
        return()
      }
      regs[unassigned] <- input$bulk_region
      rv$individual_regions <- regs
      showNotification(
        paste0(length(unassigned), " individual(s) assigned to ", input$bulk_region, "."),
        type = "message"
      )
    })

    # ── Complete ───────────────────────────────────────────────────────────
    observeEvent(input$complete, {
      regs <- rv$individual_regions %||% character(0)
      regs <- regs[nzchar(regs)]
      if (length(regs) > 0) {
        dat  <- rv$data_raw
        idx  <- match(dat$name, names(regs))
        rows <- !is.na(idx)
        dat$Study_area[rows] <- regs[idx[rows]]
        rv$data_raw <- dat
      }
      showNotification(
        paste0("Region assignment complete. ", length(regs), " individual(s) assigned."),
        type = "message", duration = 5
      )
      nav_select("main_nav", "Step 3: Flag", session = parent_session)
    })

    # ── Skip ───────────────────────────────────────────────────────────────
    observeEvent(input$skip, {
      nav_select("main_nav", "Step 3: Flag", session = parent_session)
    })

  })
}
