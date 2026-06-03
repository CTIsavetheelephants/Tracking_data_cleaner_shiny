# ---------------------------------------------------------------------------
# Spatial helpers
# ---------------------------------------------------------------------------

to_utm <- function(x4326) {
  lon  <- mean(sf::st_coordinates(x4326)[, 1], na.rm = TRUE)
  zone <- floor((lon + 180) / 6) + 1
  epsg <- if (mean(sf::st_coordinates(x4326)[, 2], na.rm = TRUE) >= 0)
    32600 + zone else 32700 + zone
  sf::st_transform(x4326, epsg)
}

# Auto-detect timezone from coordinates; overrides DRC points to Africa/Lubumbashi
get_tz_for_points <- function(lon, lat, drc_east_tz = "Africa/Lubumbashi") {
  lon <- suppressWarnings(as.numeric(lon))
  lat <- suppressWarnings(as.numeric(lat))
  bad <- !is.finite(lon) | !is.finite(lat) |
    lon <= -180 | lon >= 180 | lat <= -90 | lat >= 90 | (lon == 0 & lat == 0)
  tz   <- rep(NA_character_, length(lon))
  keep <- !bad
  if (!any(keep)) return(tz)
  tz_keep <- lutz::tz_lookup_coords(lat[keep], lon[keep], method = "fast")

  # DRC override: sample at most 500 points for the spatial join to avoid
  # crashing R on large datasets (sf::st_join against world polygons is expensive)
  keep_idx    <- which(keep)
  max_sample  <- 500L
  sample_idx  <- if (length(keep_idx) > max_sample)
    sort(sample(keep_idx, max_sample))
  else
    keep_idx

  pts_sample  <- sf::st_as_sf(
    data.frame(lon = lon[sample_idx], lat = lat[sample_idx]),
    coords = c("lon", "lat"), crs = 4326
  )
  countries <- NE_COUNTRIES %>% dplyr::select(iso_a3)
  hit       <- sf::st_join(pts_sample, countries, join = sf::st_intersects, left = TRUE)

  # If any sampled point is in DRC, override all points to the east DRC timezone
  if (any(!is.na(hit$iso_a3) & hit$iso_a3 == "COD"))
    tz_keep[tz_keep %in% c("Africa/Kinshasa", "Africa/Lubumbashi")] <- drc_east_tz

  tz[keep] <- tz_keep
  tz
}

# ---------------------------------------------------------------------------
# Column-name helpers (case-insensitive)
# ---------------------------------------------------------------------------

pick_col <- function(nms, candidates) {
  nms_l  <- tolower(nms)
  cand_l <- tolower(candidates)
  hit    <- match(cand_l, nms_l)
  if (all(is.na(hit))) return(NA_character_)
  nms[hit[which(!is.na(hit))[1]]]
}

rename_ci <- function(df, from, to) {
  nms <- names(df)
  idx <- which(tolower(nms) == tolower(from))
  if (length(idx) == 1 && !(to %in% nms)) names(df)[idx] <- to
  df
}

is_zero_wkt <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x == "" |
    stringr::str_detect(x, stringr::regex(
      "^POINT\\s*(Z|M|ZM)?\\s*\\(\\s*0+(\\.0+)?\\s+0+(\\.0+)?(?:\\s+0+(\\.0+)?)?\\s*\\)$",
      ignore_case = TRUE))
}

# ---------------------------------------------------------------------------
# Auto-detect UTM EPSG for x/y columns by trying African UTM zones
# ---------------------------------------------------------------------------

detect_utm_epsg <- function(x_vals, y_vals) {
  # Prioritise East/Central Africa zones (34-37) then expand outward
  candidates  <- c(32634:32638, 32734:32738, 32628:32633, 32728:32733)
  n           <- min(10L, length(x_vals))
  idx         <- round(seq(1, length(x_vals), length.out = n))
  pts         <- data.frame(x = x_vals[idx], y = y_vals[idx])

  africa_land <- tryCatch(
    suppressWarnings(sf::st_crop(NE_COUNTRIES,
      sf::st_bbox(c(xmin = -20, ymin = -38, xmax = 55, ymax = 40),
                  crs = sf::st_crs(4326)))),
    error = function(e) NULL
  )

  # Returns angular distance of data centroid from zone's central meridian, or NA
  .score_epsg <- function(epsg) {
    tryCatch({
      wgs    <- sf::st_transform(sf::st_as_sf(pts, coords = c("x","y"), crs = epsg), 4326)
      coords <- sf::st_coordinates(wgs)

      if (!all(coords[,1] >= -20 & coords[,1] <= 55 &
               coords[,2] >= -38 & coords[,2] <=  40)) return(NA_real_)

      if (!is.null(africa_land)) {
        centroid <- sf::st_sfc(
          sf::st_point(c(mean(coords[,1]), mean(coords[,2]))), crs = 4326
        )
        on_land <- tryCatch(lengths(sf::st_intersects(centroid, africa_land)) > 0,
                            error = function(e) TRUE)
        if (!isTRUE(on_land)) return(NA_real_)
      }

      zone_num         <- epsg %% 100
      central_meridian <- (zone_num - 1) * 6 - 177
      abs(mean(coords[, 1]) - central_meridian)
    }, error = function(e) NA_real_)
  }

  scores <- round(sapply(candidates, .score_epsg), 1)  # round to 0.1° to suppress float noise
  valid  <- !is.na(scores)
  if (!any(valid)) return(NA_integer_)
  candidates[which.min(scores)]
}

# ---------------------------------------------------------------------------
# Main prep function — handles any elephant CSV regardless of column convention
# ---------------------------------------------------------------------------

prep_elephant_csv <- function(file,
                               study_area,
                               crs_in             = 4326,
                               crs_xy             = NULL,
                               wkt_col            = "geometry",
                               default_sex        = NULL,
                               filter_col         = NULL,
                               filter_val         = NULL,
                               all_elephant       = FALSE,
                               filter_elephant    = TRUE,
                               include_na_species = FALSE,
                               timestamps_utc     = TRUE) {

  message("Processing: ", basename(file))

  # Read header to detect and skip accelerometer column (quoted arrays with
  # embedded commas are memory-heavy at scale; colClasses="NULL" skips cleanly)
  .header_names <- tryCatch(
    names(read.csv(file, nrows = 0, check.names = FALSE, stringsAsFactors = FALSE,
                   fileEncoding = "latin1")),
    error = function(e)
      names(read.csv(file, nrows = 0, check.names = FALSE, stringsAsFactors = FALSE))
  )
  .accel_cols  <- .header_names[grepl("accelero", .header_names, ignore.case = TRUE)]
  .col_classes <- setNames(rep(NA_character_, length(.header_names)), .header_names)
  .col_classes[.accel_cols] <- "NULL"

  dat <- tryCatch(
    read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
             colClasses = .col_classes, fileEncoding = "latin1"),
    error = function(e)
      read.csv(file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
               colClasses = .col_classes)
  )
  dat <- dat[, !is.na(names(dat)) & nzchar(trimws(names(dat))), drop = FALSE]

  # Optional pre-filters: e.g. region == "Zakouma", species == "elephant"
  .apply_prefilter <- function(d, col, val) {
    if (is.null(col) || !nzchar(col) || is.null(val) || !nzchar(val)) return(d)
    col_match <- names(d)[tolower(names(d)) == tolower(col)]
    if (length(col_match) == 1) {
      d <- d[d[[col_match]] == val, ]
      if (nrow(d) == 0)
        message("Skipping: no rows match ", col, " == '", val, "'")
    } else {
      message("Warning: filter column '", col, "' not found — skipping pre-filter")
    }
    d
  }
  dat <- .apply_prefilter(dat, filter_col, filter_val)
  if (nrow(dat) == 0) return(NULL)

  # Standardize key column names
  dat <- rename_ci(dat, "Sex",                         "sex")
  dat <- rename_ci(dat, "Name",                        "name")
  dat <- rename_ci(dat, "individual-local-identifier", "name")
  dat <- rename_ci(dat, "ID_Number",                   "name")
  if (!any(tolower(names(dat)) %in% "name")) dat <- rename_ci(dat, "id", "name")
  dat <- rename_ci(dat, "Tag",                  "tag")
  dat <- rename_ci(dat, "Time_Stamp",           "fixtime")
  dat <- rename_ci(dat, "Time_stamp",           "fixtime")
  dat <- rename_ci(dat, "Time Stamp",           "fixtime")
  dat <- rename_ci(dat, "Timestamp",            "fixtime")
  dat <- rename_ci(dat, "timestamp",            "fixtime")
  dat <- rename_ci(dat, "timestamp_corrected",  "fixtime")
  # Combine separate Date + Time columns when no combined timestamp column exists
  if (!"fixtime" %in% tolower(names(dat))) {
    .date_col  <- pick_col(names(dat), "date")
    .time_col2 <- pick_col(names(dat), "time")
    if (!is.na(.date_col) && !is.na(.time_col2)) {
      dat$fixtime <- paste(trimws(as.character(dat[[.date_col]])),
                           trimws(as.character(dat[[.time_col2]])))
      message("Combined '", .date_col, "' + '", .time_col2,
              "' columns into timestamp for ", basename(file))
    }
  }
  if (!("name" %in% names(dat)) && ("tag" %in% names(dat)))
    dat <- dplyr::rename(dat, name = tag)
  # Handle "name.x" / "name.y" artifacts from dplyr joins
  if (!("name" %in% names(dat))) {
    nm_x <- pick_col(names(dat), c("name.x", "name.y"))
    if (!is.na(nm_x)) names(dat)[tolower(names(dat)) == tolower(nm_x)] <- "name"
  }
  if ("name" %in% names(dat)) dat$name <- trimws(as.character(dat$name))

  # ── Species handling ───────────────────────────────────────────────────────
  # Normalise species column name to lowercase for consistent access
  sp_idx <- which(tolower(names(dat)) == "species")
  if (length(sp_idx) == 1) names(dat)[sp_idx] <- "species"

  if (!"species" %in% names(dat)) {
    # No species column — user confirmed all rows are elephant via UI checkbox
    if (!all_elephant) {
      message("Skipping ", basename(file),
              ": no species column and 'All rows are elephant' was not confirmed")
      return(NULL)
    }
    dat <- dplyr::mutate(dat, species = "elephant")

  } else {
    # Standardise all elephant-name variants to "elephant" regardless of
    # how the source data spells them (savanna, forest, Loxodonta africana, etc.)
    dat <- dat %>%
      dplyr::mutate(species = dplyr::case_when(
        grepl("elephant|loxodonta|savanna|savannah|forest", species, ignore.case = TRUE) ~ "elephant",
        TRUE ~ trimws(as.character(species))
      ))

    # Optionally treat blank / NA species rows as elephant
    if (include_na_species) {
      dat <- dat %>%
        dplyr::mutate(species = dplyr::if_else(
          is.na(species) | trimws(species) == "", "elephant", species
        ))
    }

    # Filter to elephant only if requested
    if (filter_elephant) {
      n_before <- nrow(dat)
      dat      <- dplyr::filter(dat, species == "elephant")
      n_drop   <- n_before - nrow(dat)
      if (n_drop > 0)
        message("Species filter: ", n_drop, " non-elephant row(s) removed from ",
                basename(file))
    }
  }

  dat_ele <- dat
  rm(dat); gc()

  if (nrow(dat_ele) == 0) {
    message("Skipping ", basename(file), ": no rows remain after species filter")
    return(NULL)
  }

  # Sex
  if (!("sex" %in% names(dat_ele))) dat_ele$sex <- NA_character_
  if (!is.null(default_sex))
    dat_ele$sex[is.na(dat_ele$sex) | dat_ele$sex == ""] <- default_sex
  dat_ele$sex <- trimws(tolower(as.character(dat_ele$sex)))
  dat_ele$sex <- dplyr::case_when(
    dat_ele$sex %in% c("m", "male")   ~ "male",
    dat_ele$sex %in% c("f", "female") ~ "female",
    TRUE                               ~ dat_ele$sex
  )

  # Geometry: lat/lon takes priority over WKT
  has_wkt <- wkt_col %in% names(dat_ele)
  lat_col <- pick_col(names(dat_ele), c("lat", "latitude", "location-lat", "Lat", "Latitude"))
  lon_col <- pick_col(names(dat_ele), c("lon", "long", "longitude", "location-long",
                                         "Long", "Lon", "Longitude"))

  if (!is.na(lat_col) && !is.na(lon_col)) {
    dat_ele2 <- dat_ele %>%
      dplyr::mutate(
        lat  = suppressWarnings(as.numeric(.data[[lat_col]])),
        long = suppressWarnings(as.numeric(.data[[lon_col]]))
      ) %>%
      dplyr::filter(!is.na(lat), !is.na(long),
                    long > -180, long < 180, lat > -90, lat < 90,
                    !(long == 0 & lat == 0))
    if (nrow(dat_ele2) == 0) { message("Skipping: invalid coordinates"); return(NULL) }
    dat_sf <- dat_ele2 %>%
      sf::st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
      dplyr::mutate(lon = long)
    rm(dat_ele, dat_ele2); gc()

  } else if (has_wkt) {
    dat_ele <- dat_ele %>% dplyr::filter(!is_zero_wkt(.data[[wkt_col]]))
    if (nrow(dat_ele) == 0) { message("Skipping: zero/blank WKT"); return(NULL) }
    dat_sf  <- dat_ele %>%
      sf::st_as_sf(wkt = wkt_col, crs = crs_in) %>%
      dplyr::filter(!sf::st_is_empty(geometry))
    coords  <- sf::st_coordinates(dat_sf)
    dat_sf  <- dat_sf %>%
      dplyr::mutate(lon = as.numeric(coords[, 1]),
                    lat = as.numeric(coords[, 2])) %>%
      dplyr::filter(is.finite(lon), is.finite(lat),
                    lon > -180, lon < 180, lat > -90, lat < 90,
                    !(lon == 0 & lat == 0))
    rm(dat_ele); gc()
  } else {
    # Third fallback: projected x/y (UTM easting/northing)
    x_col <- pick_col(names(dat_ele), c("x", "easting", "utm_x", "utm.x"))
    y_col <- pick_col(names(dat_ele), c("y", "northing", "utm_y", "utm.y"))

    if (!is.na(x_col) && !is.na(y_col)) {
      dat_ele2 <- dat_ele %>%
        dplyr::mutate(
          .x = suppressWarnings(as.numeric(.data[[x_col]])),
          .y = suppressWarnings(as.numeric(.data[[y_col]]))
        ) %>%
        dplyr::filter(!is.na(.x), !is.na(.y))

      if (nrow(dat_ele2) == 0) { message("Skipping: no valid x/y values"); return(NULL) }
      rm(dat_ele); gc()

      # Check if x/y values are in degree range (lon/lat) rather than UTM metres
      xy_are_degrees <- max(abs(dat_ele2$.x), na.rm = TRUE) <= 180 &&
                        max(abs(dat_ele2$.y), na.rm = TRUE) <=  90

      if (xy_are_degrees) {
        message("x/y columns appear to contain lon/lat degrees — treating as WGS84")
        dat_sf <- dat_ele2 %>%
          dplyr::filter(.x > -180, .x < 180, .y > -90, .y < 90,
                        !(.x == 0 & .y == 0)) %>%
          sf::st_as_sf(coords = c(".x", ".y"), crs = 4326, remove = FALSE) %>%
          dplyr::mutate(lon = .x, lat = .y)
        rm(dat_ele2); gc()
      } else {
        epsg <- if (!is.null(crs_xy) && !is.na(crs_xy))
          as.integer(crs_xy)
        else
          detect_utm_epsg(dat_ele2$.x, dat_ele2$.y)

        if (is.na(epsg)) {
          message("Skipping: could not auto-detect CRS for x/y columns — specify EPSG manually")
          return(NULL)
        }
        message("x/y columns detected — using EPSG ", epsg)

        wgs    <- sf::st_as_sf(dat_ele2, coords = c(".x", ".y"), crs = epsg) %>%
          sf::st_transform(4326)
        coords <- sf::st_coordinates(wgs)
        dat_sf <- wgs %>%
          dplyr::mutate(
            lon = as.numeric(coords[, 1]),
            lat = as.numeric(coords[, 2])
          ) %>%
          dplyr::filter(is.finite(lon), is.finite(lat),
                        lon > -180, lon < 180, lat > -90, lat < 90,
                        !(lon == 0 & lat == 0))
        rm(dat_ele2); gc()
      }
    } else {
      message("Skipping: no lat/lon, WKT geometry, or x/y fields found")
      return(NULL)
    }
  }

  if (nrow(dat_sf) == 0) { message("Skipping: no valid coordinates"); return(NULL) }

  # ── Standardise coordinate columns ────────────────────────────────────────
  # Drop source-specific column name variants so that bind_rows across files
  # with different column conventions produces clean, fully-populated lon/lat.
  # Keeps only the canonical "lon" and "lat" columns.
  .coord_cleanup <- unique(c(
    "long",                                                    # alias from lat/lon path
    if (!is.na(lat_col) && lat_col != "lat") lat_col else NULL, # original lat column
    if (!is.na(lon_col) && lon_col != "lon") lon_col else NULL, # original lon column
    ".x", ".y"                                                 # temps from x/y path
  ))
  .coord_cleanup <- intersect(.coord_cleanup, names(dat_sf))
  if (length(.coord_cleanup) > 0)
    dat_sf <- dplyr::select(dat_sf, -dplyr::all_of(.coord_cleanup))

  dat_sf <- dat_sf %>% dplyr::mutate(Study_area = study_area)

  # Timestamp
  time_col <- pick_col(names(dat_sf), c("fixtime", "date.time", "datetime"))
  if (is.na(time_col)) { message("Skipping: no timestamp column"); return(NULL) }
  ts_raw <- as.character(dat_sf[[time_col]])

  # When timestamps are declared as LOCAL time, strip any embedded UTC offset
  # (e.g. "+02:00", "+0200", "Z") before parsing.  Movebank exports vary: some
  # include an explicit offset in the fixtime string, some do not.  The format-
  # based parser used in Prepping_data.R (as.POSIXct with a format string) silently
  # ignores trailing offset characters, so both "18:04:28" and "18:04:28+02:00"
  # produce the same POSIXct.  lubridate::parse_date_time with "ymd HMSz" actively
  # applies the offset, shifting the UTC instant by 2 h — causing the same physical
  # fix to get two different timestamp_corrected values across files and breaking
  # deduplication.  Stripping the offset here makes the app match the merge script.
  if (!timestamps_utc) {
    ts_raw <- sub("\\s*[Zz]$", "", ts_raw)                     # trailing Z (UTC marker)
    ts_raw <- sub("\\s*[+-]\\d{2}:?\\d{2}$", "", ts_raw)       # trailing ±HH:MM or ±HHMM
  }
  ts_utc <- suppressWarnings(
    lubridate::parse_date_time(ts_raw,
      orders = c(
        # --- Day-first (European) formats tried first ---
        # Uppercase Y = 4-digit year; lowercase y = 2-digit year.
        # These must come before the ymd shorthands so that a leading day
        # value (e.g. 31) is never misread as a 2-digit year.
        "d/m/Y H:M:S",   # 31/12/2021 21:00:00
        "d/m/Y H:M",     # 31/12/2021 21:00
        "d-m-Y H:M:S",   # 31-12-2021 21:00:00
        "d-m-Y H:M",     # 31-12-2021 21:00
        "d/m/y H:M:S",   # 31/12/21 21:00:00
        "d/m/y H:M",     # 31/12/21 21:00
        "d-m-y H:M:S",   # 31-12-21 21:00:00
        "d-m-y H:M",     # 31-12-21 21:00
        # --- US formats ---
        "m/d/Y H:M:S",   # 12/31/2021 21:00:00
        "m/d/Y H:M",     # 12/31/2021 21:00
        "m/d/y H:M:S",   # 12/31/21 21:00:00
        "m/d/y H:M",     # 12/31/21 21:00
        # --- ISO / Movebank shorthand (separator-flexible, year-first) ---
        # These catch everything else: 2021-12-31, 2021/12/31, 20211231, etc.
        "ymd HMS",        # 2021-12-31 21:00:00
        "ymd HM",         # 2021-12-31 21:00
        "ymd HMSz",       # 2021-12-31 21:00:00+03:00
        "ymd IMSp",       # 12-hr clock with AM/PM
        "ymd IMp"
      ),
      tz = "UTC", train = FALSE)
  )

  # Timezone from coordinates
  tz_vec  <- get_tz_for_points(dat_sf$lon, dat_sf$lat)
  tz_tab  <- table(tz_vec, useNA = "no")
  tz_mode <- if (length(tz_tab) > 0) names(tz_tab)[which.max(tz_tab)] else "UTC"
  if (is.na(tz_mode) || !nzchar(tz_mode)) tz_mode <- "UTC"

  # UTC timestamps: with_tz shifts the display clock to local time while
  # keeping the underlying UTC instant correct (08:00 UTC → 10:00 EAT).
  # Local timestamps: the string was already local time but parsed as UTC
  # (wrong label, right clock digits); force_tz reassigns the timezone label
  # without shifting, correcting the instant (10:00 "UTC" → 10:00 EAT = 08:00 UTC).
  ts_corrected <- if (timestamps_utc)
    lubridate::with_tz(ts_utc,   tzone = tz_mode)
  else
    lubridate::force_tz(ts_utc,  tzone = tz_mode)

  dat_sf %>%
    dplyr::mutate(
      timestamp_corrected = ts_corrected,
      tz    = tz_mode,
      month = lubridate::month(timestamp_corrected),
      day   = lubridate::day(timestamp_corrected)
    ) %>%
    dplyr::filter(!is.na(timestamp_corrected)) %>%
    to_utm(.) %>%
    dplyr::mutate(
      x = sf::st_coordinates(.)[, 1],
      y = sf::st_coordinates(.)[, 2]
    ) %>%
    .dedup_consecutive("name", "timestamp_corrected", min_gap_mins = 5) %>%
    dplyr::arrange(name, timestamp_corrected)
}

# ---------------------------------------------------------------------------
# Sequential deduplication by consecutive time gap
# ---------------------------------------------------------------------------
# Keeps a fix only if it arrives at least min_gap_mins after the last kept
# fix for the same animal.  Works by walking fixes in time order and tracking
# the last-kept timestamp — so two fixes that straddle a bin boundary are
# always correctly collapsed, unlike round_date-based binning which can leave
# pairs that are (e.g.) 1 minute apart but on opposite sides of a 5-min edge.
# The earlier fix is always kept; the later one dropped when within the gap.
.dedup_consecutive <- function(df, name_col, time_col, min_gap_mins) {
  df    <- dplyr::arrange(df, .data[[name_col]], .data[[time_col]])
  nms   <- df[[name_col]]
  tms   <- df[[time_col]]
  n     <- nrow(df)
  keep  <- logical(n)
  last_t <- as.POSIXct(NA, tz = "UTC")
  last_n <- ""
  for (i in seq_len(n)) {
    if (!identical(nms[[i]], last_n)) {
      # New animal — always keep the first fix
      keep[[i]] <- TRUE
      last_n    <- nms[[i]]
      last_t    <- tms[[i]]
    } else {
      gap <- as.numeric(difftime(tms[[i]], last_t, units = "mins"))
      if (!is.na(gap) && gap >= min_gap_mins) {
        keep[[i]] <- TRUE
        last_t    <- tms[[i]]
      }
    }
  }
  df[keep, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Combine multiple prepped sf objects and deduplicate
# ---------------------------------------------------------------------------

combine_prepped <- function(prepped_list) {
  # Compute the globally-optimal UTM zone from the WGS84 lon/lat columns
  # stored in every file.  Doing this before binding means each file is
  # reprojected exactly once to the shared zone and x/y recomputed in the
  # same pass — no intermediate alignment step needed.
  all_lon     <- unlist(lapply(prepped_list, `[[`, "lon"))
  all_lat     <- unlist(lapply(prepped_list, `[[`, "lat"))
  mean_lon    <- mean(all_lon, na.rm = TRUE)
  mean_lat    <- mean(all_lat, na.rm = TRUE)
  utm_zone    <- floor((mean_lon + 180) / 6) + 1
  global_epsg <- if (mean_lat >= 0) 32600L + utm_zone else 32700L + utm_zone

  message("Global UTM zone: EPSG ", global_epsg)

  prepped_aligned <- lapply(prepped_list, function(sf_obj) {
    sf_obj        <- sf::st_transform(sf_obj, global_epsg)
    coords        <- sf::st_coordinates(sf_obj)
    sf_obj$x      <- coords[, 1]
    sf_obj$y      <- coords[, 2]
    sf_obj
  })

  # Walk fixes in time order per animal; keep a fix only if it arrives at
  # least 5 minutes after the last kept fix.  The earlier fix is always kept.
  dplyr::bind_rows(prepped_aligned) %>%
    .dedup_consecutive("name", "timestamp_corrected", min_gap_mins = 5) %>%
    dplyr::arrange(name, timestamp_corrected)
}

# ---------------------------------------------------------------------------
# Build current cleaned dataset from rv state
# ---------------------------------------------------------------------------

build_clean_data <- function(rv) {
  dat <- rv$data_flagged
  if (is.null(dat)) return(NULL)
  if (!is.null(rv$flagged_removals) && nrow(rv$flagged_removals) > 0)
    dat <- dplyr::anti_join(dat, rv$flagged_removals,
                             by = c("name", "timestamp_corrected"))
  if (!is.null(rv$manual_removals) && nrow(rv$manual_removals) > 0)
    dat <- dplyr::anti_join(dat, rv$manual_removals,
                             by = c("name", "timestamp_corrected"))
  dat
}

# ---------------------------------------------------------------------------
# Dashboard summary stats
# ---------------------------------------------------------------------------

compute_summary <- function(rv) {
  if (is.null(rv$data_raw)) return(NULL)

  raw   <- rv$data_raw
  flagd <- rv$data_flagged
  clean <- build_clean_data(rv)

  fr <- if (!is.null(rv$flagged_removals)) nrow(rv$flagged_removals) else 0
  mr <- if (!is.null(rv$manual_removals))  nrow(rv$manual_removals)  else 0

  tibble::tibble(
    Stage              = c("After automated ingest", "After flag removal", "After manual removal"),
    Individuals        = c(
      dplyr::n_distinct(raw$name),
      if (!is.null(flagd)) dplyr::n_distinct(dplyr::anti_join(
        flagd, rv$flagged_removals %||% tibble::tibble(name=character(), timestamp_corrected=POSIXct()),
        by = c("name","timestamp_corrected"))$name) else NA,
      if (!is.null(clean)) dplyr::n_distinct(clean$name) else NA
    ),
    Fixes              = c(
      nrow(raw),
      nrow(raw) - fr,
      nrow(raw) - fr - mr
    ),
    Removed            = c(0L, fr, fr + mr)
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
