############################################################
# confideR – Fisheries data simulation
#
# Generates realistic simulated fisheries CPUE datasets for
# safe use with AI coding tools. Supports common fisheries
# data patterns: lognormal catches, vessel effects, seasonal
# structure, zero-inflation, spatial areas, and observer data.
#
# Two entry points:
#   simulate_fisheries_cpue() – from parameters
#   simulate_from_fingerprint() – from a confideR fingerprint
############################################################

#' Simulate a fisheries CPUE dataset
#'
#' Generates a realistic simulated dataset matching the structure
#' of typical fisheries CPUE data, suitable for use with AI coding
#' assistants. No real data is used; all values are synthetic.
#'
#' The simulated data includes vessel effects, seasonal patterns,
#' area effects, and optionally zero-inflated catches, observer
#' data columns, and bycatch records.
#'
#' @param n_trips Number of fishing trips to simulate. Default 2000.
#' @param n_vessels Number of distinct vessels. Default 30.
#' @param n_areas Number of statistical areas. Default 8.
#' @param years Integer vector of years. Default 2010:2023.
#' @param species Character vector of target species codes.
#'   Default \code{c("species_A", "species_B", "species_C", "species_D")}.
#' @param species_probs Numeric vector of targeting probabilities
#'   (same length as \code{species}). Default \code{c(0.4, 0.3, 0.2, 0.1)}.
#' @param catch_meanlog Mean of log-catch distribution. Default \code{log(200)}.
#' @param catch_sdlog SD of log-catch distribution. Default 0.8.
#' @param effort_meanlog Mean of log-effort distribution. Default \code{log(8)}.
#' @param effort_sdlog SD of log-effort distribution. Default 0.5.
#' @param zero_inflation Proportion of zero catches. Default 0 (none).
#'   Set to e.g. 0.15 for 15\% zero catches.
#' @param include_observer Logical. Add observer programme columns?
#'   Default \code{FALSE}.
#' @param include_bycatch Logical. Add bycatch columns?
#'   Default \code{FALSE}.
#' @param bycatch_rate Per-trip probability of a bycatch event.
#'   Default 0.02.
#' @param include_spatial Logical. Add simulated lat/lon?
#'   Default \code{FALSE}. Coordinates are random within a
#'   plausible range and carry no real geographic information.
#' @param lat_range Latitude range for simulated positions.
#'   Default \code{c(-48, -34)}.
#' @param lon_range Longitude range for simulated positions.
#'   Default \code{c(166, 179)}.
#' @param vessel_effect_sd SD of random vessel effect on log-catch.
#'   Default 0.4.
#' @param area_effect_sd SD of random area effect on log-catch.
#'   Default 0.3.
#' @param seasonal_amplitude Amplitude of sinusoidal seasonal pattern
#'   on log-catch. Default 0.3 (set to 0 for no seasonality).
#' @param seed Random seed. Default 42.
#' @return A data frame with columns appropriate for CPUE standardisation.
#' @export
#' @examples
#' \dontrun{
#'   confidential_mode_on()
#'   dat <- simulate_fisheries_cpue(n_trips = 500)
#'   head(dat)
#'   write.csv(dat, "data/simulated_cpue.csv", row.names = FALSE)
#' }
simulate_fisheries_cpue <- function(
    n_trips          = 2000,
    n_vessels        = 30,
    n_areas          = 8,
    years            = 2010:2023,
    species          = c("species_A", "species_B", "species_C", "species_D"),
    species_probs    = c(0.4, 0.3, 0.2, 0.1),
    catch_meanlog    = log(200),
    catch_sdlog      = 0.8,
    effort_meanlog   = log(8),
    effort_sdlog     = 0.5,
    zero_inflation   = 0,
    include_observer = FALSE,
    include_bycatch  = FALSE,
    bycatch_rate     = 0.02,
    include_spatial  = FALSE,
    lat_range        = c(-48, -34),
    lon_range        = c(166, 179),
    vessel_effect_sd = 0.4,
    area_effect_sd   = 0.3,
    seasonal_amplitude = 0.3,
    seed             = 42
) {

  set.seed(seed)

  # Guard: simulation should happen in a confidential session to
  # reinforce the habit of activating protection before any data work.
  # This is a soft requirement — override with options(confider.require_mode_for_sim = FALSE)
  if (isTRUE(getOption("confider.require_mode_for_sim", TRUE))) {
    require_confidential_mode(
      "Activate confidential_mode_on() before generating data, or set\n  options(confider.require_mode_for_sim = FALSE) to disable this check."
    )
  }
  vessel_ids <- paste0("Vessel_", sprintf("%03d", seq_len(n_vessels)))
  area_ids   <- paste0("Area_", LETTERS[seq_len(min(n_areas, 26))])

  trip_id    <- paste0("TRIP_", sprintf("%05d", seq_len(n_trips)))
  vessel_id  <- sample(vessel_ids, n_trips, replace = TRUE)
  year       <- sample(years, n_trips, replace = TRUE)
  month      <- sample(1:12, n_trips, replace = TRUE)
  stat_area  <- sample(area_ids, n_trips, replace = TRUE)
  target_sp  <- sample(species, n_trips, replace = TRUE, prob = species_probs)

  # --- Random effects ---
  vessel_effects <- stats::rnorm(n_vessels, 0, vessel_effect_sd)
  names(vessel_effects) <- vessel_ids
  area_effects <- stats::rnorm(n_areas, 0, area_effect_sd)
  names(area_effects) <- area_ids

  v_eff <- vessel_effects[vessel_id]
  a_eff <- area_effects[stat_area]

  # --- Seasonal pattern (sinusoidal, peak in summer for southern hemisphere) ---
  seasonal <- seasonal_amplitude * sin(2 * pi * (month - 1) / 12)

  # --- Effort ---
  effort_hrs <- round(stats::rlnorm(n_trips, effort_meanlog, effort_sdlog), 1)

  # --- Catch (with vessel, area, and seasonal effects) ---
  log_mu   <- catch_meanlog + v_eff + a_eff + seasonal
  catch_kg <- round(stats::rlnorm(n_trips, log_mu, catch_sdlog), 1)

  # Zero inflation

  if (zero_inflation > 0) {
    zero_idx <- sample(n_trips, size = round(n_trips * zero_inflation))
    catch_kg[zero_idx] <- 0
  }

  # --- Environmental covariates ---
  depth_m <- round(stats::runif(n_trips, 20, 500))
  sst_c   <- round(stats::rnorm(n_trips, 14, 2.5), 1)

  # --- Derived ---
  cpue     <- ifelse(effort_hrs > 0, catch_kg / effort_hrs, NA_real_)
  log_cpue <- ifelse(catch_kg > 0, log(cpue), NA_real_)

  # --- Assemble ---
  dat <- data.frame(
    trip_id     = trip_id,
    vessel_id   = vessel_id,
    year        = as.integer(year),
    month       = as.integer(month),
    stat_area   = stat_area,
    target_sp   = target_sp,
    effort_hrs  = effort_hrs,
    catch_kg    = catch_kg,
    depth_m     = depth_m,
    sst_c       = sst_c,
    cpue        = round(cpue, 2),
    log_cpue    = round(log_cpue, 4),
    stringsAsFactors = FALSE
  )

  # --- Optional: spatial ---
  if (include_spatial) {
    dat$latitude  <- round(stats::runif(n_trips, lat_range[1], lat_range[2]), 4)
    dat$longitude <- round(stats::runif(n_trips, lon_range[1], lon_range[2]), 4)
  }

  # --- Optional: observer data ---
  if (include_observer) {
    n_observers <- max(3, round(n_vessels * 0.3))
    observer_ids <- paste0("Observer_", LETTERS[seq_len(min(n_observers, 26))])

    # ~30% of trips are observed
    observed <- stats::rbinom(n_trips, 1, 0.30)
    dat$observed    <- as.logical(observed)
    dat$observer_id <- ifelse(observed == 1,
                              sample(observer_ids, n_trips, replace = TRUE),
                              NA_character_)
  }

  # --- Optional: bycatch ---
  if (include_bycatch) {
    dat$bycatch_event <- as.logical(stats::rbinom(n_trips, 1, bycatch_rate))
    dat$bycatch_n     <- ifelse(dat$bycatch_event,
                                sample(1:3, n_trips, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
                                0L)
    dat$bycatch_sp    <- ifelse(dat$bycatch_event,
                                sample(c("protected_sp_1", "protected_sp_2", "protected_sp_3"),
                                       n_trips, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
                                NA_character_)
  }

  # --- Introduce realistic missingness ---
  # ~2% missing SST, ~1% missing depth
  dat$sst_c[sample(n_trips, round(n_trips * 0.02))]   <- NA_real_
  dat$depth_m[sample(n_trips, round(n_trips * 0.01))]  <- NA_integer_

  message(sprintf(
    "[confideR] Simulated %d trips, %d vessels, %d areas, %d years. Safe to use with AI tools.",
    n_trips, n_vessels, n_areas, length(years)
  ))

  dat
}


#' Generate simulated data from a fingerprint
#'
#' Creates a synthetic dataset that matches the structure described by
#' a \code{confider_fingerprint}: same column names (or aliases),
#' types, and approximate distributional properties. Useful for
#' generating AI-safe data that mirrors your real dataset.
#'
#' @param fp A \code{confider_fingerprint} object (must have
#'   \code{mode = "summary"}).
#' @param n Number of rows. Default uses the original dataset's row count.
#' @param seed Random seed. Default 42.
#' @return A data frame with the same structure as the fingerprinted data.
#' @export
simulate_from_fingerprint <- function(fp, n = NULL, seed = 42) {
  stopifnot(is_fingerprint(fp))
  if (is.null(fp$summary)) {
    stop("[confideR] Fingerprint must have mode='summary'. Re-run fingerprint() with mode='summary'.",
         call. = FALSE)
  }

  set.seed(seed)
  n <- n %||% fp$meta$n_rows

  dat <- data.frame(row_id = seq_len(n))


  for (i in seq_len(nrow(fp$columns))) {
    col <- fp$columns[i, ]
    alias <- col$alias
    type  <- col$type
    s     <- fp$summary[[alias]]
    stats <- s$stats

    col_data <- switch(type,
      continuous = {
        if (!is.null(stats) && !is.null(stats$mean)) {
          round(stats::rnorm(n, stats$mean, stats$sd %||% 1), 2)
        } else {
          stats::rnorm(n)
        }
      },
      integer = {
        if (!is.null(stats) && !is.null(stats$mean)) {
          as.integer(round(stats::rnorm(n, stats$mean, stats$sd %||% 1)))
        } else {
          sample(1:100, n, replace = TRUE)
        }
      },
      categorical = {
        if (!is.null(stats$levels)) {
          sample(stats$levels, n, replace = TRUE)
        } else {
          nl <- stats$n_levels %||% 5
          sample(paste0("level_", seq_len(nl)), n, replace = TRUE)
        }
      },
      date = {
        if (!is.null(stats$min) && !is.null(stats$max)) {
          min_d <- tryCatch(as.Date(paste0(stats$min, "-01")), error = function(e) as.Date("2010-01-01"))
          max_d <- tryCatch(as.Date(paste0(stats$max, "-28")), error = function(e) as.Date("2023-12-31"))
          as.Date(sample(as.integer(min_d):as.integer(max_d), n, replace = TRUE), origin = "1970-01-01")
        } else {
          as.Date(sample(0:1825, n, replace = TRUE), origin = "2018-01-01")
        }
      },
      logical = {
        p <- if (!is.null(stats$p_true)) stats$p_true else 0.5
        as.logical(stats::rbinom(n, 1, p))
      },
      # default
      rep(NA, n)
    )

    # Inject missingness
    if (col$p_miss > 0) {
      n_na <- round(n * col$p_miss)
      if (n_na > 0) {
        col_data[sample(n, n_na)] <- NA
      }
    }

    dat[[alias]] <- col_data
  }

  dat$row_id <- NULL

  message(sprintf(
    "[confideR] Simulated %d rows x %d cols from fingerprint. Safe to use with AI tools.",
    n, ncol(dat)
  ))

  dat
}
