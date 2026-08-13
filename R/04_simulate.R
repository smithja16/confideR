############################################################
# confideR – Generic survey data simulation
#
# Generates realistic simulated datasets for safe use with
# AI coding tools. Supports common ecological, environmental,
# and social survey data patterns: lognormal responses,
# group effects, seasonal structure, zero-inflation, spatial
# areas, observer coverage, and secondary observations.
#
# Two entry points:
#   simulate_data()              – from parameters
#   simulate_from_fingerprint()  – from a confideR fingerprint
############################################################

#' Simulate a generic survey or monitoring dataset
#'
#' Generates a realistic simulated dataset matching the structure
#' of typical ecological, environmental, or social survey data,
#' suitable for use with AI coding assistants. No real data is
#' used; all values are synthetic.
#'
#' The simulated data includes group effects (e.g. vessel, farm,
#' household), strata effects (e.g. statistical area, region),
#' seasonal patterns, and optionally zero-inflated responses,
#' observer coverage columns, secondary observations (e.g.
#' bycatch, incidental records), and spatial coordinates.
#'
#' Column names in the output are intentionally generic
#' (\code{obs_id}, \code{group_id}, \code{stratum},
#' \code{category}, \code{response}, \code{rate}) so the
#' simulated dataset works as a stand-in for any structured
#' survey response with effort standardisation. Rename columns
#' to match your real data before writing analysis code.
#'
#' @param n_obs Number of observations (e.g. trips, surveys,
#'   records) to simulate. Default 2000.
#' @param n_groups Number of distinct sampling units that recur
#'   across observations (e.g. vessels, farms, households).
#'   Default 30.
#' @param n_strata Number of spatial or categorical strata
#'   (e.g. statistical areas, regions, land-use classes).
#'   Default 8.
#' @param years Integer vector of years. Default \code{2010:2023}.
#' @param categories Character vector of category labels
#'   (e.g. target species, land-use type, survey theme).
#'   Default \code{c("category_A","category_B","category_C","category_D")}.
#' @param category_probs Numeric vector of selection probabilities
#'   (same length as \code{categories}).
#'   Default \code{c(0.4, 0.3, 0.2, 0.1)}.
#' @param response_meanlog Mean of the log-response distribution.
#'   Default \code{log(200)}.
#' @param response_sdlog SD of the log-response distribution.
#'   Default 0.8.
#' @param effort_meanlog Mean of the log-effort distribution.
#'   Default \code{log(8)}.
#' @param effort_sdlog SD of the log-effort distribution.
#'   Default 0.5.
#' @param zero_inflation Proportion of zero responses. Default 0
#'   (none). Set to e.g. 0.15 for 15 percent zeros.
#' @param include_observer Logical. Add observer coverage columns
#'   (\code{observed}, \code{observer_id})? Default \code{FALSE}.
#' @param include_secondary Logical. Add secondary observation
#'   columns (e.g. bycatch, incidental records)?
#'   Default \code{FALSE}.
#' @param secondary_rate Per-observation probability of a
#'   secondary observation event. Default 0.02.
#' @param include_spatial Logical. Add simulated lat/lon?
#'   Default \code{FALSE}. Coordinates are random within
#'   \code{lat_range}/\code{lon_range} and carry no real
#'   geographic information.
#' @param lat_range Latitude range. Default \code{c(-50, 50)}.
#' @param lon_range Longitude range. Default \code{c(-150, 150)}.
#' @param group_effect_sd SD of the random group effect on
#'   log-response. Default 0.4.
#' @param strata_effect_sd SD of the random strata effect on
#'   log-response. Default 0.3.
#' @param seasonal_amplitude Amplitude of sinusoidal seasonal
#'   pattern on log-response. Default 0.3 (set to 0 for none).
#' @param seed Random seed. Default 42.
#' @return A data frame with columns appropriate for rate
#'   standardisation or mixed-effects modelling.
#' @note The relationships in the output (group, strata, seasonal, and
#'   effort-driven effects) are those built in by the parameters, not learned
#'   from any real dataset — which is what makes the result safe to share.
#'   To reproduce the joint structure of a specific confidential dataset,
#'   use the \pkg{synthpop} package on your secure machine rather than trying
#'   to encode correlations in a fingerprint.
#' @seealso [simulate_from_fingerprint()] to instead generate synthetic
#'   data that mirrors a real dataset's structure (the
#'   "fingerprint -> simulate" round trip), and [fingerprint()] to create
#'   that structural summary.
#' @export
#' @examples
#' \dontrun{
#'   confidential_mode_on()
#'   dat <- simulate_data(n_obs = 500)
#'   head(dat)
#'   write.csv(dat, "data/simulated_data.csv", row.names = FALSE)
#' }
simulate_data <- function(
    n_obs              = 2000,
    n_groups           = 30,
    n_strata           = 8,
    years              = 2010:2023,
    categories         = c("category_A", "category_B", "category_C", "category_D"),
    category_probs     = c(0.4, 0.3, 0.2, 0.1),
    response_meanlog   = log(200),
    response_sdlog     = 0.8,
    effort_meanlog     = log(8),
    effort_sdlog       = 0.5,
    zero_inflation     = 0,
    include_observer   = FALSE,
    include_secondary  = FALSE,
    secondary_rate     = 0.02,
    include_spatial    = FALSE,
    lat_range          = c(-50, 50),
    lon_range          = c(-150, 150),
    group_effect_sd    = 0.4,
    strata_effect_sd   = 0.3,
    seasonal_amplitude = 0.3,
    seed               = 117
) {

  set.seed(seed)

  # Guard: simulation should happen in a confidential session to
  # reinforce the habit of activating protection before any data work.
  # Override with options(confider.require_mode_for_sim = FALSE).
  if (isTRUE(getOption("confider.require_mode_for_sim", TRUE))) {
    require_confidential_mode(
      "Activate confidential_mode_on() before generating data, or set\n  options(confider.require_mode_for_sim = FALSE) to disable this check."
    )
  }

  group_ids  <- paste0("Group_",  sprintf("%03d", seq_len(n_groups)))
  strata_ids <- paste0("Strata_", LETTERS[seq_len(min(n_strata, 26))])

  obs_id   <- paste0("OBS_", sprintf("%05d", seq_len(n_obs)))
  group_id <- sample(group_ids,  n_obs, replace = TRUE)
  year     <- sample(years,      n_obs, replace = TRUE)
  month    <- sample(1:12,       n_obs, replace = TRUE)
  stratum  <- sample(strata_ids, n_obs, replace = TRUE)
  category <- sample(categories, n_obs, replace = TRUE, prob = category_probs)

  # --- Random effects ---
  group_effects  <- stats::rnorm(n_groups, 0, group_effect_sd)
  names(group_effects) <- group_ids
  strata_effects <- stats::rnorm(n_strata, 0, strata_effect_sd)
  names(strata_effects) <- strata_ids

  g_eff <- group_effects[group_id]
  s_eff <- strata_effects[stratum]

  # --- Seasonal pattern (sinusoidal) ---
  seasonal <- seasonal_amplitude * sin(2 * pi * (month - 1) / 12)

  # --- Effort ---
  effort <- round(stats::rlnorm(n_obs, effort_meanlog, effort_sdlog), 1)

  # --- Response (with group, strata, and seasonal effects) ---
  log_mu   <- response_meanlog + g_eff + s_eff + seasonal
  response <- round(stats::rlnorm(n_obs, log_mu, response_sdlog), 1)

  if (zero_inflation > 0) {
    zero_idx <- sample(n_obs, size = round(n_obs * zero_inflation))
    response[zero_idx] <- 0
  }

  # --- Generic continuous covariates (illustrative; replace with domain-
  #     appropriate equivalents in real analyses) ---
  covariate_1 <- round(stats::runif(n_obs, 20, 500))     # e.g. depth, elevation, distance
  covariate_2 <- round(stats::rnorm(n_obs, 14, 2.5), 1)  # e.g. temperature, index score

  # --- Derived rate ---
  rate     <- ifelse(effort > 0, response / effort, NA_real_)
  log_rate <- ifelse(response > 0, log(rate), NA_real_)

  # --- Assemble core data frame ---
  dat <- data.frame(
    obs_id      = obs_id,
    group_id    = as.factor(group_id),
    year        = as.integer(year),
    month       = as.integer(month),
    stratum     = as.factor(stratum),
    category    = as.factor(category),
    effort      = effort,
    response    = response,
    covariate_1 = covariate_1,
    covariate_2 = covariate_2,
    rate        = round(rate, 2),
    log_rate    = round(log_rate, 4),
    stringsAsFactors = FALSE
  )

  # --- Optional: spatial ---
  if (include_spatial) {
    dat$latitude  <- round(stats::runif(n_obs, lat_range[1], lat_range[2]), 4)
    dat$longitude <- round(stats::runif(n_obs, lon_range[1], lon_range[2]), 4)
  }

  # --- Optional: observer coverage ---
  if (include_observer) {
    n_observers <- max(3, round(n_groups * 0.3))
    observer_ids <- paste0("Observer_", LETTERS[seq_len(min(n_observers, 26))])
    observed <- stats::rbinom(n_obs, 1, 0.30)
    dat$observed    <- as.logical(observed)
    dat$observer_id <- ifelse(observed == 1,
                              sample(observer_ids, n_obs, replace = TRUE),
                              NA_character_)
  }

  # --- Optional: secondary observations (e.g. bycatch, incidental records) ---
  if (include_secondary) {
    dat$secondary_event    <- as.logical(stats::rbinom(n_obs, 1, secondary_rate))
    dat$secondary_count    <- ifelse(dat$secondary_event,
                                     sample(1:3, n_obs, replace = TRUE,
                                            prob = c(0.7, 0.2, 0.1)),
                                     0L)
    dat$secondary_category <- ifelse(dat$secondary_event,
                                     sample(c("secondary_sp_1", "secondary_sp_2",
                                              "secondary_sp_3"),
                                            n_obs, replace = TRUE,
                                            prob = c(0.5, 0.3, 0.2)),
                                     NA_character_)
  }

  # --- Introduce realistic missingness ---
  dat$covariate_2[sample(n_obs, round(n_obs * 0.02))] <- NA_real_
  dat$covariate_1[sample(n_obs, round(n_obs * 0.01))] <- NA_integer_

  message(sprintf(
    "[confideR] Simulated %d observations, %d groups, %d strata, %d years. Safe to use with AI tools.",
    n_obs, n_groups, n_strata, length(years)
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
#' This is the second half of the "fingerprint -> simulate" round trip:
#' \code{fingerprint(real_data)} produces a privacy-safe summary, and
#' \code{simulate_from_fingerprint()} turns that summary back into a
#' synthetic dataset you can safely develop against with AI tools.
#'
#' @param fp A \code{confider_fingerprint} object (must have
#'   \code{mode = "summary"}).
#' @param n Number of rows. Default uses the original dataset's row count.
#' @param seed Random seed. Default 117.
#' @return A data frame with the same structure as the fingerprinted data.
#' @note Columns are generated independently from their marginal summaries;
#'   the output does not reproduce correlations or other joint relationships
#'   between variables. This is deliberate — a fingerprint is a non-disclosive
#'   structural summary, and encoding joint structure would push real
#'   information into an object meant to be shareable. For the same reason,
#'   continuous columns are resampled from a normal distribution using the
#'   reported mean and standard deviation, so skewed non-negative variables
#'   (e.g. catch, biomass, income) can yield negative simulated values —
#'   treat the simulated data as structural rather than physical, or clamp
#'   values where your analysis requires it. If realistic
#'   relationships matter (e.g. to check that an analysis recovers known
#'   effects), use [simulate_data()], which builds in group, strata, seasonal,
#'   and effort-driven structure by design. To reproduce a specific real
#'   dataset's own joint structure, generate synthetic data with the
#'   \pkg{synthpop} package on your secure machine (where the real data lives)
#'   and transfer the synthetic file — keeping joint information out of the
#'   fingerprint.
#' @seealso [fingerprint()] to create the summary this consumes,
#'   [restore_names()] to map an obfuscated simulation's alias column names
#'   back to the originals in-session, and [simulate_data()] to generate
#'   data from parameters instead.
#' @export
simulate_from_fingerprint <- function(fp, n = NULL, seed = 117) {
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
          # Fingerprint dates arrive at day ("2016-06-03"), month ("2016-06"),
          # or year ("2016") precision depending on anonymise$date_precision;
          # complete each to a full date before parsing (previously the year
          # form errored into the 2010/2023 fallback and silently shifted the
          # simulated range).
          min_d <- tryCatch(.complete_fp_date(stats$min, "-01", "-01"),
                            error = function(e) as.Date("2010-01-01"))
          max_d <- tryCatch(.complete_fp_date(stats$max, "-12", "-28"),
                            error = function(e) as.Date("2023-12-31"))
          as.Date(sample(as.integer(min_d):as.integer(max_d), n, replace = TRUE), origin = "1970-01-01")
        } else {
          as.Date(sample(0:1825, n, replace = TRUE), origin = "2018-01-01")
        }
      },
      logical = {
        p <- if (!is.null(stats$p_true)) stats$p_true else 0.5
        as.logical(stats::rbinom(n, 1, p))
      },
      rep(NA, n)
    )

    if (col$p_miss > 0) {
      n_na <- round(n * col$p_miss)
      if (n_na > 0) col_data[sample(n, n_na)] <- NA
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

# Complete a fingerprint date string to a full "%Y-%m-%d" date. Fingerprints
# record dates at day, month, or year precision (anonymise$date_precision);
# month_part/day_part supply the missing components (e.g. "-01"/"-01" for a
# range minimum, "-12"/"-28" for a maximum).
.complete_fp_date <- function(s, month_part, day_part) {
  s <- as.character(s)
  if (grepl("^\\d{4}$", s))       s <- paste0(s, month_part)
  if (grepl("^\\d{4}-\\d{2}$", s)) s <- paste0(s, day_part)
  out <- as.Date(s)
  if (is.na(out)) stop("unparseable fingerprint date: ", s)
  out
}
