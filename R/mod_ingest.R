mod_ingest_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),

    # Left: inputs
    card(
      card_header("Data Ingestion"),
      tags$style("
        .ingest-left label        { font-size: 0.8rem !important; }
        .ingest-left .form-control{ font-size: 0.8rem !important; }
        .ingest-left .help-block  { font-size: 0.75rem !important; }
        .ingest-left h6           { font-size: 0.8rem !important; }
        .ingest-left .btn         { font-size: 0.85rem !important; }
        #ingest-preview table     { font-size: 0.8rem !important; }
        .ingest-left input[type='checkbox'] {
          border: 2px solid #555 !important;
          width: 1rem !important;
          height: 1rem !important;
        }
        /* Pre-filter pair: compact side-by-side dropdowns */
        .prefilter-pair label                       { font-size: 0.76rem !important; margin-bottom: 0.1rem !important; }
        .prefilter-pair .selectize-input            { font-size: 0.76rem !important; min-height: 28px !important; }
        .prefilter-pair .selectize-dropdown .option { font-size: 0.76rem !important; padding: 3px 6px !important; }
        .prefilter-pair .form-group                 { margin-bottom: 0.25rem !important; }
        /* Post-prepare optional sections */
        .post-prepare label                       { font-size: 0.78rem !important; }
        .post-prepare select                      { font-size: 0.78rem !important; }
        .post-prepare .selectize-input            { font-size: 0.78rem !important; }
        .post-prepare .selectize-dropdown .option { font-size: 0.78rem !important; }
      "),
      tags$script(HTML("
        /* ── Prevent page-scroll jump caused by file-input dialogs ────────────
         *
         * With bslib / Bootstrap 5, Shiny renders fileInput as a plain
         * <input type='file'> with the browser's native Choose-file button.
         * Clicking it makes the browser scroll the element into view immediately
         * (before any 'change' event).  Two further scrolls can follow:
         *   - when the OS file dialog closes and the window regains focus, and
         *   - when Shiny re-renders uiOutput(name_corr_col_ui) after processing.
         *
         * Fix:
         *   1. mousedown on input[type=file] → save position, then enforce it
         *      every 50 ms for 300 ms (overrides the immediate scroll-to-element).
         *   2. window 'focus' (dialog closed) → restore position.
         *   3. 'change' → restore position + short-delay backup.
         *   4. shiny:value within 3 s → restore (catches uiOutput re-render).
         */
        $(document).on('mousedown', 'input[type=file]', function() {
          var pos = window.scrollY || window.pageYOffset;
          window._fileInputScrollY    = pos;
          window._fileInputScrollTime = Date.now();

          /* Lock scroll at ~60 fps until the OS dialog opens (window blur).
           * This overrides the browser's scroll-to-element during the
           * click/focus sequence before the dialog appears. */
          var locked = true;
          var lockTimer = setInterval(function() {
            if (locked) window.scrollTo(0, pos);
          }, 16);

          function onBlur() {
            locked = false;
            clearInterval(lockTimer);
            window.removeEventListener('blur', onBlur);
            /* Restore when dialog closes and window regains focus */
            function onFocus() {
              window.removeEventListener('focus', onFocus);
              window.scrollTo(0, pos);
              setTimeout(function() { window.scrollTo(0, pos); }, 150);
              setTimeout(function() { window.scrollTo(0, pos); }, 500);
            }
            window.addEventListener('focus', onFocus);
          }
          window.addEventListener('blur', onBlur);
          /* Safety: release lock after 2 s even if blur never fires */
          setTimeout(function() { locked = false; clearInterval(lockTimer); }, 2000);
        });

        $(document).on('change', 'input[type=file]', function() {
          var pos = window._fileInputScrollY;
          if (pos === undefined) return;
          window.scrollTo(0, pos);
          setTimeout(function() { window.scrollTo(0, pos); }, 200);
          setTimeout(function() { window.scrollTo(0, pos); }, 600);
        });

        /* Restore whenever Shiny pushes a new value while browse was recent */
        $(document).on('shiny:value', function() {
          if (window._fileInputScrollY === undefined) return;
          if (Date.now() - (window._fileInputScrollTime || 0) > 3000) return;
          var pos = window._fileInputScrollY;
          setTimeout(function() { window.scrollTo(0, pos); }, 50);
        });

        /* Smooth scroll to a DOM element by id */
        Shiny.addCustomMessageHandler('scrollToId', function(data) {
          setTimeout(function() {
            var el = document.getElementById(data.id);
            if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }, 400);
        });
      ")),
      div(class = "ingest-left",
      fileInput(ns("files"), "Upload raw CSV file(s)",
                multiple = TRUE, accept = ".csv"),
      uiOutput(ns("per_file_ui")),
      hr(),
      textInput(ns("study_area"), "Study area name", placeholder = "e.g. Garamba National Park"),
      hr(),
      h6("x/y coordinate files (optional)"),
      numericInput(ns("crs_xy"), "Input CRS EPSG (leave blank to auto-detect)",
                   value = NA),
      helpText("Only needed if your file has x/y UTM columns instead of lat/lon.",
               "Auto-detection tries common African UTM zones.",
               "Example: 32734 = WGS84 / UTM Zone 34S."),
      hr(),
      h6("Name corrections from CSV (optional)"),
      helpText("Upload a two-column CSV mapping raw collar / data names to canonical",
               "elephant names. Applied to each file before they are combined,",
               "so the rolling deduplication correctly treats renamed animals as one."),
      actionButton(ns("open_corr_modal"), "Browse corrections CSV…",
                   class = "btn-outline-secondary w-100",
                   icon  = icon("file-csv")),
      uiOutput(ns("name_corr_status_ui")),
      uiOutput(ns("name_corr_col_ui")),
      hr(),
      tags$p(class = "text-muted",
             style = "font-size:0.78rem; font-weight:600; margin-bottom:4px;",
             "Applied automatically on preparation:"),
      tags$ol(style = "font-size:0.78rem; padding-left:1.2rem; color:#6c757d; margin-bottom:0.5rem;",
        tags$li("Apply name corrections (if CSV uploaded above)"),
        tags$li("Standardise column names"),
        tags$li("Remove rows with blank or NA individual name"),
        tags$li("Filter to elephant species"),
        tags$li("Standardise sex values"),
        tags$li("Parse & validate lat/lon coordinates (removes NAs, out-of-range values, and (0, 0) fixes)"),
        tags$li("Parse timestamps"),
        tags$li("Detect timezone from coordinates"),
        tags$li("Project to UTM (EPSG auto-calculated from data extent)"),
        tags$li("Split EarthRanger individuals with multiple grouping IDs (groupby_col) whose date ranges overlap (e.g. Louise→Louise_1, Louise_2) — sequential recollaring with no overlap is left intact; only applies if EarthRanger checkbox is ticked"),
        tags$li("Remove duplicate fixes"),
        tags$li("Remove fixes within 5 minutes of the previous fix per individual")
      ),
      hr(),
      actionButton(ns("run"), "Prepare data", class = "btn-primary w-100"),
      tags$div(id = ns("post_prep_anchor")),
      uiOutput(ns("sex_correction_ui")),
      uiOutput(ns("complete_btn_ui"))
      ) # close ingest-left div
    ),

    # Right: summary boxes + preview, then separate individual summary card
    tagList(
      card(
        fill = FALSE,
        card_header("Preview"),
        uiOutput(ns("summary_boxes")),
        DTOutput(ns("preview"))
      ),
      uiOutput(ns("ind_summary_ui"))
    )
  )
}

.utm_label <- function(dat) {
  epsg <- sf::st_crs(dat)$epsg
  if (is.na(epsg)) return("Unknown")
  if (epsg >= 32601 && epsg <= 32660) return(paste0("UTM Zone ", epsg - 32600, "N"))
  if (epsg >= 32701 && epsg <= 32760) return(paste0("UTM Zone ", epsg - 32700, "S"))
  as.character(epsg)
}

mod_ingest_server <- function(id, rv, parent_session) {
  moduleServer(id, function(input, output, session) {

    # Per-file sample cache: list[[i]] holds up to 100k rows of file i.
    # Populated once on upload; used in-memory for species detection + filter_val lookups.
    file_samples <- reactiveVal(list())

    # Holds the uploaded name corrections CSV (data.frame).
    name_corr_data <- reactiveVal(NULL)

    # Flips to TRUE once on first prepare; never goes back — keeps the name
    # corrections section stable so re-running corrections doesn't reset the fileInput.
    data_prepared <- reactiveVal(FALSE)

    # Per-file summary: file name | raw rows | after per-file prep | final (after cross-file dedup)
    ingest_file_summary <- reactiveVal(NULL)

    # ── Reset when Start Over clears rv$study_area ─────────────────────────
    observeEvent(rv$study_area, {
      req(is.null(rv$study_area))
      file_samples(list())
      ingest_file_summary(NULL)
      updateTextInput(session,  "study_area", value = "")
      updateNumericInput(session, "crs_xy",   value = NA)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ── Read all uploaded files into cache ─────────────────────────────────
    observeEvent(input$files, {
      n_files <- nrow(input$files)
      withProgress(message = "Reading uploaded files…", value = 0, {
        samps <- vector("list", n_files)
        for (i in seq_len(n_files)) {
          setProgress(i / n_files,
                      detail = paste0("File ", i, " of ", n_files,
                                      ": ", input$files$name[i]))
          samps[[i]] <- tryCatch(
            readr::read_csv(
              input$files$datapath[i],
              col_types      = readr::cols(.default = readr::col_character()),
              locale         = readr::locale(encoding = "latin1"),
              show_col_types = FALSE,
              n_max          = 100000
            ),
            error = function(e) NULL
          )
        }
        file_samples(samps)
      })
    })

    # ── Per-file UI section ────────────────────────────────────────────────
    output$per_file_ui <- renderUI({
      req(!is.null(input$files))
      fnames <- input$files$name
      samps  <- file_samples()   # list; may be empty while upload is processing

      tagList(
        hr(),
        h6("Per-file settings"),
        helpText(
          style = "margin-bottom:0.75rem;",
          "Review timestamp and species settings for each uploaded file.",
          "An optional pre-filter lets you subset by any column before preparation."
        ),

        lapply(seq_along(fnames), function(i) {
          samp     <- if (i <= length(samps)) samps[[i]] else NULL
          all_cols <- if (!is.null(samp)) names(samp) else character(0)

          # Species detection
          has_sp_col <- !is.null(samp) && "species" %in% tolower(all_cols)
          still_load <- is.null(samp)

          sp_vals <- character(0)
          n_blank <- 0L
          if (isTRUE(has_sp_col)) {
            sp_col  <- all_cols[tolower(all_cols) == "species"][1]
            sp_raw  <- as.character(samp[[sp_col]])
            sp_vals <- sort(unique(sp_raw[!is.na(sp_raw) & nzchar(trimws(sp_raw))]))
            n_blank <- sum(is.na(samp[[sp_col]]) | !nzchar(trimws(sp_raw)))
          }

          div(
            style = paste0(
              "border:1px solid #dee2e6; border-radius:6px; ",
              "padding:0.75rem; margin-bottom:0.75rem;"
            ),

            # File name header
            tags$p(
              style = "font-weight:600; font-size:0.85rem; margin-bottom:0.6rem;",
              tags$span(style = "margin-right:0.35rem;", "\U0001F4C4"),
              fnames[i]
            ),

            # ── Timestamps ──────────────────────────────────────────────
            checkboxInput(
              session$ns(paste0("timestamps_utc_", i)),
              "Timestamps are in UTC (uncheck if already stored in local time)",
              value = TRUE
            ),

            # ── EarthRanger collar split (only shown when groupby_col present) ──
            if (!is.null(samp) && "groupby_col" %in% names(samp)) {
              tagList(
                tags$hr(style = "margin:0.5rem 0;"),
                checkboxInput(
                  session$ns(paste0("earthranger_", i)),
                  "EarthRanger data — split individuals with multiple grouping IDs (groupby_col) with overlapping date ranges (sequential recollaring is left intact)",
                  value = TRUE
                )
              )
            },

            tags$hr(style = "margin:0.5rem 0;"),

            # ── Species ─────────────────────────────────────────────────
            if (still_load) {
              helpText("Detecting columns…")
            } else if (isFALSE(has_sp_col)) {
              tagList(
                helpText("No species column detected in this file."),
                checkboxInput(
                  session$ns(paste0("all_elephant_", i)),
                  "All rows in this file are elephant",
                  value = FALSE
                )
              )
            } else {
              tagList(
                {
                  is_ele  <- grepl("elephant|loxodonta|savanna|savannah|forest", sp_vals, ignore.case = TRUE)
                  ele_sp  <- sp_vals[ is_ele]
                  nonele  <- sp_vals[!is_ele]
                  parts   <- list("Species values: ")
                  if (length(ele_sp) > 0)
                    parts <- c(parts, list(tags$b(style = "color:#2d7a2d;",
                                                  paste(ele_sp, collapse = ", "))))
                  if (length(ele_sp) > 0 && length(nonele) > 0)
                    parts <- c(parts, list(", "))
                  if (length(nonele) > 0)
                    parts <- c(parts, list(
                      tags$b(style = "color:#c0392b;",
                             paste(nonele, collapse = ", ")),
                      tags$span(style = "color:#c0392b; font-size:0.74rem;",
                                " ← will be filtered out")
                    ))
                  do.call(tags$p,
                          c(list(style = "font-size:0.8rem; margin-bottom:0.2rem;"),
                            parts))
                },
                if (n_blank > 0)
                  tags$p(
                    class = "text-warning",
                    style = "font-size:0.8rem;",
                    n_blank, " row(s) have blank / NA species."
                  ),
                checkboxInput(
                  session$ns(paste0("filter_elephant_", i)),
                  "Filter to elephant rows only",
                  value = TRUE
                ),
                checkboxInput(
                  session$ns(paste0("include_na_species_", i)),
                  "Include rows where species is blank / NA (treat as elephant)",
                  value = FALSE
                )
              )
            },

            # ── Pre-filter ───────────────────────────────────────────────
            if (length(all_cols) > 0) {
              tagList(
                tags$hr(style = "margin:0.5rem 0;"),
                div(
                  class = "prefilter-pair",
                  style = "display:flex; gap:0.5rem; align-items:flex-start;",
                  div(style = "flex:1; min-width:0;",
                    selectizeInput(
                      session$ns(paste0("filter_col_", i)),
                      "Pre-filter column",
                      choices  = c("--- none ---" = "", all_cols),
                      selected = "",
                      width    = "100%",
                      options  = list(placeholder = "--- none ---",
                                      maxOptions   = 300)
                    )
                  ),
                  div(style = "flex:1; min-width:0;",
                    uiOutput(session$ns(paste0("filter_val_ui_", i)))
                  )
                )
              )
            }
          )  # end div
        })  # end lapply
      )
    })

    # ── Dynamic filter_val dropdowns — one observer per file ──────────────
    # We use observe() + local() so each closure captures its own `fi`.
    # The observe fires whenever file_samples() updates (new uploads) and
    # registers renderUI outputs for however many files are present.
    observe({
      req(input$files)
      n_files <- nrow(input$files)
      samps   <- file_samples()

      lapply(seq_len(n_files), function(i) {
        local({
          fi <- i
          output[[paste0("filter_val_ui_", fi)]] <- renderUI({
            col_sel <- input[[paste0("filter_col_", fi)]]
            if (is.null(col_sel) || !nzchar(col_sel)) return(NULL)

            samp <- if (fi <= length(samps)) samps[[fi]] else NULL
            if (is.null(samp) || !col_sel %in% names(samp)) return(NULL)

            vals <- c("--- select ---" = "",
                      sort(unique(as.character(samp[[col_sel]]))))
            selectizeInput(
              session$ns(paste0("filter_val_", fi)),
              "Filter to value",
              choices  = vals,
              selected = "",
              width    = "100%"
            )
          })
        })
      })
    })

    # ── Run ────────────────────────────────────────────────────────────────
    observeEvent(input$run, {
      n_files <- if (!is.null(input$files)) nrow(input$files) else 0L
      samps   <- file_samples()

      # --- Validation -------------------------------------------------------
      missing <- character(0)

      if (is.null(input$files))
        missing <- c(missing, "Upload at least one CSV file")

      if (!nzchar(trimws(input$study_area %||% "")))
        missing <- c(missing, "Enter a study area name")

      for (i in seq_len(n_files)) {
        samp       <- if (i <= length(samps)) samps[[i]] else NULL
        has_sp_col <- !is.null(samp) && "species" %in% tolower(names(samp))
        if (!has_sp_col && !isTRUE(input[[paste0("all_elephant_", i)]])) {
          missing <- c(missing,
            paste0("File ‘", input$files$name[i],
                   "’: no species column detected — ",
                   "tick ‘All rows are elephant’ to proceed"))
        }
      }

      if (length(missing) > 0) {
        showNotification(
          tags$div(
            tags$b("Please complete the following:"),
            tags$ul(lapply(missing, tags$li))
          ),
          type = "error", duration = 8
        )
        return()
      }

      # --- Prepare ----------------------------------------------------------
      tryCatch(
        withProgress(message = "Preparing data...", {
          file_paths <- input$files$datapath
          file_names <- input$files$name

          raw_row_counts    <- integer(length(file_paths))
          prepped_row_counts <- integer(length(file_paths))

          prepped_list <- lapply(seq_along(file_paths), function(i) {
            samp       <- if (i <= length(samps)) samps[[i]] else NULL
            has_sp_col <- !is.null(samp) && "species" %in% tolower(names(samp))

            all_elephant       <- !has_sp_col &&
                                  isTRUE(input[[paste0("all_elephant_", i)]])
            filter_elephant    <-  has_sp_col &&
                                  isTRUE(input[[paste0("filter_elephant_", i)]])
            include_na_species <-  has_sp_col && filter_elephant &&
                                  isTRUE(input[[paste0("include_na_species_", i)]])

            fcol <- trimws(input[[paste0("filter_col_", i)]] %||% "")
            fval <- trimws(input[[paste0("filter_val_", i)]] %||% "")

            # ── Capture actual row count from the full file ─────────────────
            raw_row_counts[[i]] <<- tryCatch({
              d <- readr::read_csv(
                file_paths[i],
                col_select     = 1,
                col_types      = readr::cols(.default = readr::col_character()),
                locale         = readr::locale(encoding = "latin1"),
                show_col_types = FALSE
              )
              if (nzchar(fcol) && nzchar(fval)) {
                samp_i <- if (i <= length(samps)) samps[[i]] else NULL
                if (!is.null(samp_i) && fcol %in% names(samp_i)) {
                  # read full file with filter column to get accurate filtered count
                  d2 <- tryCatch(readr::read_csv(
                    file_paths[i],
                    col_select     = dplyr::all_of(fcol),
                    col_types      = readr::cols(.default = readr::col_character()),
                    locale         = readr::locale(encoding = "latin1"),
                    show_col_types = FALSE
                  ), error = function(e) NULL)
                  if (!is.null(d2))
                    return(sum(as.character(d2[[fcol]]) == fval, na.rm = TRUE))
                }
              }
              nrow(d)
            }, error = function(e) NA_integer_)

            tryCatch({
              prepped <- prep_elephant_csv(
                file_paths[i],
                study_area         = trimws(input$study_area),
                filter_col         = if (nzchar(fcol)) fcol else NULL,
                filter_val         = if (nzchar(fval)) fval else NULL,
                crs_xy             = input$crs_xy,
                all_elephant       = all_elephant,
                filter_elephant    = filter_elephant,
                include_na_species = include_na_species,
                timestamps_utc     = isTRUE(input[[paste0("timestamps_utc_", i)]]),
                earthranger        = isTRUE(input[[paste0("earthranger_", i)]])
              )
              # Apply name corrections per-file so that combine_prepped sees
              # canonical names and its rolling 5-minute dedup correctly merges
              # near-duplicate fixes from sources that used different name conventions.
              corr_d   <- name_corr_data()
              raw_col  <- trimws(input$raw_name_col       %||% "")
              can_col  <- trimws(input$canonical_name_col %||% "")
              if (!is.null(prepped) && !is.null(corr_d) &&
                  nzchar(raw_col) && raw_col != "— select —" &&
                  nzchar(can_col) && can_col != "— select —" &&
                  raw_col %in% names(corr_d) && can_col %in% names(corr_d)) {
                corr_tbl <- corr_d %>%
                  dplyr::select(data_name = dplyr::all_of(raw_col),
                                canonical = dplyr::all_of(can_col)) %>%
                  dplyr::filter(!is.na(data_name), nzchar(trimws(data_name)),
                                !is.na(canonical),  nzchar(trimws(canonical))) %>%
                  dplyr::distinct(data_name, .keep_all = TRUE)
                prepped <- prepped %>%
                  dplyr::left_join(corr_tbl, by = c("name" = "data_name")) %>%
                  dplyr::mutate(name = dplyr::coalesce(canonical, name)) %>%
                  dplyr::select(-canonical)
              }
              prepped_row_counts[[i]] <<- if (!is.null(prepped)) nrow(prepped) else 0L
              prepped
            },
            error = function(e) {
              message("ERROR in prep_elephant_csv (", file_names[i], "): ",
                      conditionMessage(e))
              NULL
            })
          })

          prepped_list <- Filter(Negate(is.null), prepped_list)

          if (length(prepped_list) == 0) {
            showNotification("No valid data could be prepared.", type = "error")
            return()
          }

          dat <- combine_prepped(prepped_list)

          ingest_file_summary(
            tibble::tibble(
              File          = file_names,
              `CSV rows`    = raw_row_counts,
              `After prep`  = prepped_row_counts
            ) %>%
              dplyr::bind_rows(
                tibble::tibble(
                  File         = "TOTAL after cross-file dedup",
                  `CSV rows`   = sum(raw_row_counts),
                  `After prep` = nrow(dat)
                )
              )
          )

          gaps_h <- dat %>%
            sf::st_drop_geometry() %>%
            dplyr::arrange(name, timestamp_corrected) %>%
            dplyr::group_by(name) %>%
            dplyr::mutate(gap_h = as.numeric(difftime(timestamp_corrected,
                                                      dplyr::lag(timestamp_corrected),
                                                      units = "hours"))) %>%
            dplyr::filter(!is.na(gap_h)) %>%
            dplyr::pull(gap_h)
          rv$median_fix_interval <- round(median(gaps_h, na.rm = TRUE), 1)

          rv$data_raw   <- dat
          rv$study_area <- trimws(input$study_area)
          data_prepared(TRUE)
          session$sendCustomMessage("scrollToId",
                                    list(id = session$ns("post_prep_anchor")))
        }),
        error = function(e) {
          msg <- conditionMessage(e)
          message("FATAL in Prepare data: ", msg)
          showNotification(
            tags$div(tags$b("Preparation failed:"), tags$br(), msg),
            type = "error", duration = 15
          )
        }
      )
    })

    # ── Summary boxes ──────────────────────────────────────────────────────
    output$summary_boxes <- renderUI({
      req(rv$data_raw)
      dat <- rv$data_raw

      tz_val <- {
        tz_tab <- table(dat$tz, useNA = "no")
        if (length(tz_tab) > 0) names(tz_tab)[which.max(tz_tab)] else "UTC"
      }

      date_range <- paste0(
        format(min(dat$timestamp_corrected), "%b %Y"), " – ",
        format(max(dat$timestamp_corrected), "%b %Y")
      )

      med_interval <- rv$median_fix_interval %||% NA
      med_label    <- if (!is.na(med_interval)) paste0(med_interval, " hrs") else "—"

      top_boxes <- list(
        value_box(
          title    = "Individuals",
          value    = dplyr::n_distinct(dat$name),
          showcase = tags$span("\U0001F418", style = "font-size:2rem"),
          theme    = "primary"
        ),
        value_box(
          title    = "Fixes",
          value    = format(nrow(dat), big.mark = ","),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = "primary"
        ),
        value_box(
          title    = "Date range",
          value    = date_range,
          showcase = bsicons::bs_icon("calendar-range"),
          theme    = "primary"
        ),
        value_box(
          title    = "Median fix interval",
          value    = med_label,
          showcase = bsicons::bs_icon("clock-history"),
          theme    = "primary"
        )
      )

      tagList(
        tags$style("
          .compact-ingest .bslib-value-box { min-height: unset !important; height: 135px !important; }
          .compact-ingest .bslib-value-box .card-body { padding: 0.6rem 0.75rem !important; }
          .compact-ingest .value-box-value   { font-size: 1.05rem !important; line-height: 1.3 !important; }
          .compact-ingest .value-box-title   { font-size: 1.05rem !important; }
          .compact-ingest .value-box-showcase { width: 2rem !important; font-size: 0.85rem !important; }
          .compact-ingest .value-box-showcase svg { width: 1rem !important; height: 1rem !important; }
        "),
        div(class = "compact-ingest",
          do.call(layout_columns, c(
            list(col_widths = rep(floor(12 / length(top_boxes)), length(top_boxes)),
                 style      = "margin-bottom: 0.5rem;"),
            top_boxes
          ))
        ),
        div(class = "compact-ingest",
          layout_columns(
            col_widths = c(6, 6),
            style      = "margin-bottom: 1rem;",
            value_box(
              title    = "Projection",
              value    = .utm_label(dat),
              showcase = bsicons::bs_icon("globe"),
              theme    = "secondary"
            ),
            value_box(
              title    = "Timezone",
              value    = tz_val,
              showcase = bsicons::bs_icon("clock"),
              theme    = "secondary"
            )
          )
        )
      )
    })

    # ── Preview table ──────────────────────────────────────────────────────
    output$preview <- renderDT({
      req(rv$data_raw)
      rv$data_raw %>%
        sf::st_drop_geometry() %>%
        dplyr::slice_head(n = 15)
    }, options = list(
         dom          = "t",
         scrollX      = TRUE,
         fixedColumns = list(leftColumns = 1)
       ),
       extensions = "FixedColumns",
       class = "compact cell-border stripe")

    # ── Per-individual summary (shown after preparation) ───────────────────
    output$ind_summary_ui <- renderUI({
      req(rv$data_raw)
      tagList(
        card(
          card_header("Automated preparation summary"),
          DTOutput(session$ns("file_summary_tbl"))
        ),
        card(
          card_header("Per-individual summary"),
          DTOutput(session$ns("ind_summary"))
        )
      )
    })

    output$file_summary_tbl <- renderDT({
      req(ingest_file_summary())
      ingest_file_summary()
    }, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
       rownames = FALSE, class = "compact cell-border stripe")

    output$ind_summary <- renderDT({
      req(rv$data_raw)
      rv$data_raw %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          Fixes = dplyr::n(),
          From  = format(min(timestamp_corrected, na.rm = TRUE), "%Y-%m-%d"),
          To    = format(max(timestamp_corrected, na.rm = TRUE), "%Y-%m-%d"),
          .groups = "drop"
        ) %>%
        dplyr::rename(Individual = name)
    }, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
       rownames = FALSE, class = "compact cell-border stripe")

    # ── Individual pickers — re-renders when rv$data_raw changes so the lists
    # always reflect canonical names after corrections or exclusions are applied.
    output$sex_correction_ui <- renderUI({
      req(rv$data_raw)
      inds <- sort(unique(rv$data_raw$name))
      div(class = "post-prepare",
        # ── Scroll-up hint ─────────────────────────────────────────────
        tags$p(
          class = "text-muted",
          style = "font-size:0.76rem; margin-top:0.6rem; margin-bottom:0;",
          tags$em("↑ Scroll up to the Preview panel on the right to see the data summary.")
        ),

        # ── Remove individuals ─────────────────────────────────────────
        hr(),
        h6("Remove individuals (Optional)"),
        helpText("Select individuals to remove entirely from the dataset.",
                 "Apply name corrections above first if needed — the list will",
                 "then show canonical names."),
        selectizeInput(session$ns("exclude_inds"), NULL,
                       choices  = inds,
                       selected = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "Select individuals to remove…")),
        actionButton(session$ns("apply_exclusions"),
                     "Remove selected individuals",
                     class = "btn-danger w-100 mt-1"),

        # ── Manual one-off rename ──────────────────────────────────────
        hr(),
        h6("Correct individual name (one-off) (Optional)"),
        helpText("For anything not covered by the corrections CSV.",
                 "Renames the selected individual across all rows."),
        selectInput(session$ns("name_ind"), "Individual to rename",
                    choices = c("— select —" = "", inds)),
        textInput(session$ns("new_name"), "New name",
                  placeholder = "e.g. Elephant01"),
        actionButton(session$ns("apply_name_fix"), "Apply name correction",
                     class = "btn-warning w-100 mt-1"),

        # ── Sex corrections ────────────────────────────────────────────
        hr(),
        h6("Correct sex assignments (Optional)"),
        helpText("Select an individual to correct their sex across all rows."),
        selectInput(session$ns("sex_ind"), "Individual",
                    choices = c("— select —" = "", inds)),
        uiOutput(session$ns("current_sex_display")),
        selectInput(session$ns("new_sex"), "Corrected sex",
                    choices = c("— select —" = "", "male", "female", "unknown")),
        actionButton(session$ns("apply_sex_fix"), "Apply sex correction",
                     class = "btn-warning w-100 mt-1")
      )
    })

    # ── Name corrections: open modal so the file picker doesn't scroll the page
    observeEvent(input$open_corr_modal, {
      showModal(modalDialog(
        title = "Upload name corrections CSV",
        helpText("Two-column CSV: one column with the name as it appears in",
                 "the data, one column with the correct canonical name."),
        fileInput(session$ns("name_corr_file"), NULL,
                  accept      = ".csv",
                  buttonLabel = "Browse…",
                  placeholder = "No file selected"),
        easyClose = TRUE,
        footer    = modalButton("Close")
      ))
    })

    # ── Name corrections CSV: read on upload ───────────────────────────────
    observeEvent(input$name_corr_file, {
      req(input$name_corr_file)
      d <- tryCatch(
        readr::read_csv(
          input$name_corr_file$datapath,
          col_types      = readr::cols(.default = readr::col_character()),
          show_col_types = FALSE
        ),
        error = function(e) {
          showNotification(paste0("Could not read corrections file: ",
                                  conditionMessage(e)),
                           type = "error")
          NULL
        }
      )
      name_corr_data(d)
      removeModal()   # close the modal — main page never scrolled
    })

    # ── Name corrections: status line shown after upload ───────────────────
    output$name_corr_status_ui <- renderUI({
      d <- name_corr_data()
      if (is.null(d)) return(NULL)
      fname <- input$name_corr_file$name %||% "corrections file"
      tags$p(class = "text-success",
             style = "font-size:0.8rem; margin: 0.3rem 0 0;",
             "✓ ", tags$b(fname),
             tags$span(class = "text-muted",
                        paste0(" — ", nrow(d), " row(s) loaded")))
    })

    # ── Name corrections: column pickers (shown once a CSV is uploaded) ───────
    output$name_corr_col_ui <- renderUI({
      d <- name_corr_data()
      if (is.null(d)) return(NULL)
      cols <- names(d)
      tagList(
        selectInput(session$ns("raw_name_col"),
                    "Column with the name as it appears in the data",
                    choices = c("— select —" = "", cols)),
        selectInput(session$ns("canonical_name_col"),
                    "Column with the correct name to use",
                    choices = c("— select —" = "", cols)),
        helpText("Corrections are applied to each file when 'Prepare data' is clicked.")
      )
    })

    # ── Remove individuals ─────────────────────────────────────────────────
    observeEvent(input$apply_exclusions, {
      req(rv$data_raw, length(input$exclude_inds) > 0)
      to_remove <- input$exclude_inds
      dat <- rv$data_raw %>% dplyr::filter(!name %in% to_remove)
      rv$data_raw <- dat
      showNotification(
        paste0(length(to_remove), " individual(s) removed. ",
               dplyr::n_distinct(dat$name), " individual(s) remain."),
        type = "warning", duration = 5
      )
    })

    output$current_sex_display <- renderUI({
      req(rv$data_raw, input$sex_ind, nzchar(input$sex_ind %||% ""))
      sexes <- unique(rv$data_raw$sex[rv$data_raw$name == input$sex_ind])
      sexes <- sexes[!is.na(sexes) & nzchar(sexes)]
      tags$p(class = "text-muted", style = "font-size:0.82rem; margin: 0.25rem 0 0.5rem;",
             "Current sex in data: ",
             tags$b(if (length(sexes) == 0) "(none/NA)" else paste(sexes, collapse = ", ")))
    })

    observeEvent(input$apply_name_fix, {
      req(rv$data_raw,
          nzchar(input$name_ind %||% ""),
          nzchar(trimws(input$new_name %||% "")))
      old_name <- input$name_ind
      new_name <- trimws(input$new_name)
      if (old_name == new_name) {
        showNotification("New name is the same as the current name.", type = "warning")
        return()
      }
      dat <- rv$data_raw
      dat$name[dat$name == old_name] <- new_name
      rv$data_raw <- dat
      showNotification(paste0("'", old_name, "' renamed to '", new_name, "'."),
                       type = "message", duration = 4)
      updateTextInput(session, "new_name", value = "")
    })

    observeEvent(input$apply_sex_fix, {
      req(rv$data_raw,
          nzchar(input$sex_ind %||% ""),
          nzchar(input$new_sex %||% ""),
          input$sex_ind != "— select —",
          input$new_sex  != "— select —")
      dat <- rv$data_raw
      dat$sex[dat$name == input$sex_ind] <- input$new_sex
      rv$data_raw <- dat
      showNotification(
        paste0("Sex for '", input$sex_ind, "' corrected to '", input$new_sex, "'."),
        type = "message", duration = 4
      )
    })

    # ── Complete button ────────────────────────────────────────────────────
    output$complete_btn_ui <- renderUI({
      req(rv$data_raw)
      tagList(
        hr(),
        actionButton(session$ns("complete"), "Complete → Step 2: Region Assignment",
                     class = "btn-outline-primary w-100")
      )
    })

    observeEvent(input$complete, {
      req(rv$data_raw)
      nav_select("main_nav", "Step 2: Region Assignment", session = parent_session)
    })

  })
}
