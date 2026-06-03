# ---------------------------------------------------------------------------
# Flagging utilities — adapted from Code/ECF/utils.R
# All functions use name_col / time_col auto-detection (lowercase first).
# ---------------------------------------------------------------------------

.resolve_col <- function(sf_obj, provided, candidates, role) {
  if (!is.null(provided)) {
    if (!provided %in% names(sf_obj))
      stop(role, " column '", provided, "' not found")
    return(provided)
  }
  found <- intersect(candidates, names(sf_obj))
  if (length(found) == 0)
    stop("Could not auto-detect ", role, " column")
  found[1]
}

flag_by_speed <- function(sf_obj,
                           name_col          = NULL,
                           time_col          = NULL,
                           speed_vehicle_kmh = 25,
                           speed_air_kmh     = 150) {
  name_col <- .resolve_col(sf_obj, name_col, c("name", "Name"), "name")
  time_col <- .resolve_col(sf_obj, time_col, c("timestamp_corrected", "Time_Stamp"), "time")
  sf_obj   <- sf_obj %>% dplyr::arrange(.data[[name_col]], .data[[time_col]])

  # Use pre-extracted UTM x/y for distance — avoids duplicating the sf geometry
  # column (lag(geometry) on 100k rows causes severe memory pressure)
  result <- sf_obj %>%
    dplyr::group_by(.data[[name_col]]) %>%
    dplyr::mutate(
      .x_prev        = dplyr::lag(x),
      .y_prev        = dplyr::lag(y),
      .time_prev     = dplyr::lag(.data[[time_col]]),
      step_m         = sqrt((x - .x_prev)^2 + (y - .y_prev)^2),
      step_s         = as.numeric(difftime(.data[[time_col]], .time_prev, units = "secs")),
      step_speed_kmh = dplyr::if_else(!is.na(step_m) & !is.na(step_s) & step_s > 0,
                                      (step_m / 1000) / (step_s / 3600),
                                      NA_real_),
      speed_flag     = dplyr::case_when(
        is.na(step_speed_kmh)               ~ "first_fix",
        step_speed_kmh >= speed_air_kmh     ~ "suspicious_airborne",
        step_speed_kmh >= speed_vehicle_kmh ~ "suspicious_vehicle",
        TRUE                                ~ "ok"
      )
    ) %>%
    dplyr::select(-.x_prev, -.y_prev, -.time_prev) %>%
    dplyr::ungroup()

  message("flag_by_speed: ",
          sum(result$speed_flag == "suspicious_vehicle",  na.rm = TRUE), " suspicious_vehicle, ",
          sum(result$speed_flag == "suspicious_airborne", na.rm = TRUE), " suspicious_airborne")
  result
}

detect_predeployment <- function(sf_obj,
                                  name_col               = NULL,
                                  time_col               = NULL,
                                  max_elephant_speed_kmh = 15,
                                  min_valid_fixes        = 5,
                                  max_predep_days        = 30,
                                  immob_radius_m         = 50,
                                  immob_min_days         = 5) {
  name_col <- .resolve_col(sf_obj, name_col, c("name", "Name"), "name")
  time_col <- .resolve_col(sf_obj, time_col, c("timestamp_corrected", "Time_Stamp"), "time")
  if (!"step_speed_kmh" %in% names(sf_obj))
    stop("Run flag_by_speed() before detect_predeployment()")

  # For each individual, search only within the first max_predep_days calendar
  # days from their first fix (collar sitting in camp before deployment).
  coords_mat <- sf::st_coordinates(sf_obj)
  names_vec  <- sf_obj[[name_col]]
  times_vec  <- sf_obj[[time_col]]

  .early_immob_cut <- function(ind) {
    idx   <- which(names_vec == ind)
    n     <- length(idx)
    sub_c <- coords_mat[idx, , drop = FALSE]
    sub_t <- times_vec[idx]
    # Hard cap: only look within max_predep_days from first fix
    n_cap <- sum(sub_t <= sub_t[1] + max_predep_days * 86400)
    if (n_cap < 2L) return(0L)
    last_i       <- 0L
    search_start <- 1L
    # Iteratively chain staging clusters within the fixed day cap.
    repeat {
      if (search_start >= n_cap) break
      found <- 0L
      i <- search_start
      while (i <= n_cap && found == 0L) {
        ax <- sub_c[i, 1]; ay <- sub_c[i, 2]
        j <- i + 1L
        while (j <= n_cap) {
          dx <- sub_c[j, 1] - ax; dy <- sub_c[j, 2] - ay
          if (sqrt(dx * dx + dy * dy) > immob_radius_m) break
          j <- j + 1L
        }
        stay_end <- j - 1L
        if (stay_end > i) {
          dur <- as.numeric(difftime(sub_t[stay_end], sub_t[i], units = "days"))
          if (dur >= immob_min_days) found <- stay_end
        }
        if (found == 0L) i <- max(j, i + 1L)
      }
      if (found == 0L) break
      last_i       <- found
      search_start <- found + 1L
    }
    last_i
  }

  cuts   <- vapply(unique(names_vec), .early_immob_cut, integer(1L))
  cut_df <- stats::setNames(
    data.frame(names(cuts), as.integer(cuts), stringsAsFactors = FALSE),
    c(name_col, ".immob_cut")
  )
  sf_obj <- dplyr::left_join(sf_obj, cut_df, by = name_col)

  result <- sf_obj %>%
    dplyr::group_by(.data[[name_col]]) %>%
    dplyr::mutate(
      predeployment_auto = {
        spd   <- step_speed_kmh
        flags <- speed_flag
        times <- .data[[time_col]]
        n     <- length(spd)
        predep <- rep(FALSE, n)
        # Cap speed-flag search to first max_predep_days from first fix
        n_cap <- sum(times <= times[1] + max_predep_days * 86400)
        last_sus_all <- max(which(flags %in% c("suspicious_vehicle", "suspicious_airborne")), 0)
        last_sus <- if (last_sus_all <= n_cap) last_sus_all else 0
        last_sus <- max(last_sus, .immob_cut[1L])
        start_i  <- last_sus + 1
        if (start_i > n) {
          predep <- rep(TRUE, n)
        } else {
          for (i in start_i:n) {
            end_i  <- min(i + min_valid_fixes - 1, n)
            window <- spd[i:end_i]
            if (length(window) == min_valid_fixes &&
                all(is.na(window) | window <= max_elephant_speed_kmh)) {
              if (i > 1) predep[seq_len(i - 1)] <- TRUE
              break
            }
          }
        }
        predep
      }
    ) %>%
    dplyr::select(-.immob_cut) %>%
    dplyr::ungroup()

  n_total <- sum(result$predeployment_auto, na.rm = TRUE)
  message("detect_predeployment: ", n_total, " pre-deployment fix(es) flagged")
  result
}

detect_shift_episodes <- function(sf_obj,
                                   name_col     = NULL,
                                   time_col     = NULL,
                                   shift_m      = 10000,
                                   max_gap_hours = 48) {
  name_col <- .resolve_col(sf_obj, name_col, c("name", "Name"), "name")
  time_col <- .resolve_col(sf_obj, time_col, c("timestamp_corrected", "Time_Stamp"), "time")

  # Use pre-extracted UTM x/y for distances — avoids lag/lead on the sf geometry
  # column which creates 2 full copies of 100k geometries simultaneously
  with_steps <- sf_obj %>%
    dplyr::arrange(.data[[name_col]], .data[[time_col]]) %>%
    dplyr::group_by(.data[[name_col]]) %>%
    dplyr::mutate(
      .x_prev   = dplyr::lag(x),
      .y_prev   = dplyr::lag(y),
      .x_next   = dplyr::lead(x),
      .y_next   = dplyr::lead(y),
      .t_prev   = dplyr::lag(.data[[time_col]]),
      .t_next   = dplyr::lead(.data[[time_col]]),
      .d_prev   = sqrt((x - .x_prev)^2 + (y - .y_prev)^2),
      .d_next   = sqrt((x - .x_next)^2 + (y - .y_next)^2),
      .gap_prev = as.numeric(difftime(.data[[time_col]], .t_prev, units = "hours")),
      .gap_next = as.numeric(difftime(.t_next, .data[[time_col]], units = "hours")),
      shift_boundary = !is.na(.d_prev) & .d_prev > shift_m &
                       !is.na(.d_next) & .d_next > shift_m &
                       !is.na(.gap_prev) & .gap_prev <= max_gap_hours &
                       !is.na(.gap_next) & .gap_next <= max_gap_hours
    ) %>%
    dplyr::select(-.x_prev, -.y_prev, -.x_next, -.y_next,
                  -.t_prev, -.t_next, -.d_prev, -.d_next,
                  -.gap_prev, -.gap_next) %>%
    dplyr::ungroup()

  # Each isolated erroneous fix becomes its own single-point episode
  episodes <- with_steps %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(shift_boundary) %>%
    dplyr::select(dplyr::all_of(c(name_col, time_col))) %>%
    dplyr::mutate(ep_start = .data[[time_col]],
                  ep_end   = .data[[time_col]]) %>%
    dplyr::select(dplyr::all_of(name_col), ep_start, ep_end)

  message("detect_shift_episodes: ", nrow(episodes), " isolated erroneous fix(es) across ",
          dplyr::n_distinct(episodes[[name_col]]), " individual(s)")
  list(with_steps = with_steps, episodes = episodes)
}

# Marks each fix in sf_obj with shift_episode = TRUE if it falls within an episode window
mark_shift_episodes <- function(sf_obj, episodes, name_col = NULL, time_col = NULL) {
  name_col <- .resolve_col(sf_obj, name_col, c("name", "Name"), "name")
  time_col <- .resolve_col(sf_obj, time_col, c("timestamp_corrected", "Time_Stamp"), "time")

  if (nrow(episodes) == 0) {
    sf_obj$shift_episode <- FALSE
    return(sf_obj)
  }

  marked <- sf_obj %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(name_col, time_col))) %>%
    dplyr::left_join(episodes, by = name_col, relationship = "many-to-many") %>%
    dplyr::mutate(in_ep = !is.na(ep_start) &
                    .data[[time_col]] >= ep_start &
                    .data[[time_col]] <= ep_end) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(name_col, time_col)))) %>%
    dplyr::summarise(shift_episode = any(in_ep), .groups = "drop")

  sf_obj %>%
    dplyr::left_join(marked, by = c(name_col, time_col)) %>%
    dplyr::mutate(shift_episode = tidyr::replace_na(shift_episode, FALSE))
}


flag_immobility <- function(sf_obj,
                             name_col      = NULL,
                             time_col      = NULL,
                             radius_m      = 50,
                             min_days      = 5,
                             min_fixes     = 3,
                             end_fraction  = 0.3) {
  name_col <- .resolve_col(sf_obj, name_col, c("name", "Name"), "name")
  time_col <- .resolve_col(sf_obj, time_col, c("timestamp_corrected", "Time_Stamp"), "time")

  sf_obj <- sf_obj %>% dplyr::arrange(.data[[name_col]], .data[[time_col]])

  # Per-individual helper.
  # A qualifying cluster (within radius_m, lasting >= min_days) is only flagged
  # if it falls in the LAST end_fraction of the track — i.e. fewer than
  # end_fraction of all fixes remain after the cluster ends.
  # Clusters in the middle of the track (genuine rests) are left completely
  # unflagged.
  #
  # When multiple qualifying terminal clusters exist (e.g. the elephant settled
  # at location B, moved, then died at location A), the EARLIEST qualifying
  # cluster sets terminal_start. Everything from that point to end-of-track is
  # flagged — including any movement between clusters — because all of it falls
  # within the user-defined terminal window.
  .flag_one <- function(coords, times) {
    n <- nrow(coords)
    immobile <- rep(FALSE, n)
    i <- 1L
    terminal_start <- NA_integer_

    while (i <= n) {
      ax <- coords[i, 1]; ay <- coords[i, 2]
      j <- i + 1L
      while (j <= n) {
        dx <- coords[j, 1] - ax; dy <- coords[j, 2] - ay
        if (sqrt(dx * dx + dy * dy) > radius_m) break
        j <- j + 1L
      }
      stay_end <- j - 1L
      if (stay_end > i) {
        dur        <- as.numeric(difftime(times[stay_end], times[i], units = "days"))
        n_in_clust <- stay_end - i + 1L
        if (dur >= min_days && n_in_clust >= min_fixes) {
          remaining_frac <- (n - stay_end) / n
          if (remaining_frac <= end_fraction && is.na(terminal_start)) {
            terminal_start <- i   # first qualifying cluster; do not overwrite
          }
        }
      }
      i <- max(j, i + 1L)
    }

    if (!is.na(terminal_start)) {
      immobile[terminal_start:n] <- TRUE   # first terminal cluster + everything after
    }
    immobile
  }

  coords_mat <- sf::st_coordinates(sf_obj)
  names_vec  <- sf_obj[[name_col]]
  times_vec  <- sf_obj[[time_col]]
  immobile   <- rep(FALSE, nrow(sf_obj))

  has_predep <- "predeployment_auto" %in% names(sf_obj)

  for (ind in unique(names_vec)) {
    idx <- which(names_vec == ind)
    imm <- .flag_one(coords_mat[idx, , drop = FALSE], times_vec[idx])
    if (has_predep) imm <- imm & !sf_obj$predeployment_auto[idx]
    immobile[idx] <- imm
  }

  sf_obj$immobility <- immobile
  message("flag_immobility: ", sum(immobile), " fix(es) in immobility episodes across ",
          dplyr::n_distinct(names_vec[immobile]), " individual(s)")
  sf_obj
}

flag_hq <- function(sf_obj, hq_lon, hq_lat, hq_radius_m) {
  hq_pt  <- sf::st_sfc(sf::st_point(c(hq_lon, hq_lat)), crs = 4326) %>%
    sf::st_transform(sf::st_crs(sf_obj))
  hq_buf <- sf::st_buffer(hq_pt, hq_radius_m)
  sf_obj$hq_flag <- lengths(sf::st_within(sf_obj$geometry, hq_buf)) > 0
  message("flag_hq: ", sum(sf_obj$hq_flag), " fix(es) within HQ buffer")
  sf_obj
}

flag_outside_bbox <- function(sf_obj, lon_min, lon_max, lat_min, lat_max) {
  bbox_poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(lon_min, lat_min), c(lon_max, lat_min),
      c(lon_max, lat_max), c(lon_min, lat_max),
      c(lon_min, lat_min)
    ))),
    crs = 4326
  ) %>% sf::st_transform(sf::st_crs(sf_obj))
  sf_obj$outside_bbox <- lengths(sf::st_within(sf_obj$geometry, bbox_poly)) == 0
  message("flag_outside_bbox: ", sum(sf_obj$outside_bbox), " fix(es) outside bounding box")
  sf_obj
}

# Build a point_color column on an sf object for leaflet rendering.
# Expects hq_flag and outside_bbox columns to exist (set to FALSE if not used).
assign_flag_colour <- function(dat) {
  dat %>%
    dplyr::mutate(
      point_color = dplyr::case_when(
        shift_episode                             ~ FLAG_COLOURS[["shift_episode"]],
        outside_bbox                              ~ FLAG_COLOURS[["outside_bbox"]],
        hq_flag                                   ~ FLAG_COLOURS[["hq"]],
        predeployment_auto                        ~ FLAG_COLOURS[["predeployment"]],
        immobility                                ~ FLAG_COLOURS[["immobility"]],
        speed_flag == "suspicious_airborne"       ~ FLAG_COLOURS[["suspicious_airborne"]],
        speed_flag == "suspicious_vehicle"        ~ FLAG_COLOURS[["suspicious_vehicle"]],
        TRUE                                      ~ FLAG_COLOURS[["ok"]]
      ),
      flag_type = dplyr::case_when(
        shift_episode                             ~ "shift_episode",
        outside_bbox                              ~ "outside_bbox",
        hq_flag                                   ~ "hq",
        predeployment_auto                        ~ "predeployment",
        immobility                                ~ "immobility",
        speed_flag == "suspicious_airborne"       ~ "suspicious_airborne",
        speed_flag == "suspicious_vehicle"        ~ "suspicious_vehicle",
        TRUE                                      ~ "ok"
      )
    )
}
