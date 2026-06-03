source("global.R")

ui <- page_navbar(
  title    = "Elephant Tracking Data Cleaner",
  theme    = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  id       = "main_nav",
  fillable = FALSE,

  nav_panel("Step 1: Ingest",            mod_ingest_ui("ingest")),
  nav_panel("Step 2: Region Assignment", mod_assign_ui("assign")),
  nav_panel("Step 3: Flag",              mod_flag_ui("flag")),
  nav_panel("Step 4: Flag Review",       mod_review_ui("review")),
  nav_panel("Step 5: Manual Cleaning",   mod_clean_ui("clean")),
  nav_panel("Step 6: Export",            mod_export_ui("export")),
  nav_panel("Dashboard",               mod_dashboard_ui("dashboard")),
  nav_spacer(),
  nav_item(
    actionButton("start_over", "Start over", icon = icon("rotate-left"),
                 class = "btn-sm",
                 style = "color: #ffffff; border-color: #ffffff; background: transparent;")
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    data_raw            = NULL,
    data_flagged        = NULL,
    flagged_removals    = NULL,
    manual_removals     = NULL,
    study_area          = NULL,
    site_config         = list(),
    shift_episodes      = NULL,
    median_fix_interval = NULL,
    individual_regions  = NULL,
    hq_point            = NULL,   # list(lon, lat) or NULL — set in flag module
    bbox                = NULL    # list(lon_min, lon_max, lat_min, lat_max) or NULL
  )

  observeEvent(input$start_over, {
    rv$data_raw            <- NULL
    rv$data_flagged        <- NULL
    rv$flagged_removals    <- NULL
    rv$manual_removals     <- NULL
    rv$study_area          <- NULL
    rv$site_config         <- list()
    rv$shift_episodes      <- NULL
    rv$median_fix_interval <- NULL
    rv$individual_regions  <- NULL
    rv$hq_point            <- NULL
    rv$bbox                <- NULL
    gc(); gc()  # two passes to release C-level sf/GEOS structures
    nav_select("main_nav", "Step 1: Ingest", session = session)
    showNotification("All data cleared. Ready for a new file.", type = "message")
  })

  mod_ingest_server("ingest",       rv, session)
  mod_assign_server("assign",       rv, session)
  mod_flag_server("flag",           rv, session)
  mod_review_server("review",       rv, session)
  mod_clean_server("clean",         rv, session)
  mod_export_server("export",       rv, session)
  mod_dashboard_server("dashboard", rv)
}

shinyApp(ui, server)
