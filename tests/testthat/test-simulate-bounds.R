# simulate_from_fingerprint() previously drew numeric columns from
# rnorm(mean, sd), producing negative catches, losing zero-inflation, and
# pushing bounded integers (months, counts) outside their range.

make_catch_data <- function(n = 4000) {
  set.seed(3)
  data.frame(
    catch_kg = ifelse(runif(n) < 0.35, 0, round(rlnorm(n, log(40), 1.2), 1)),
    hooks    = rpois(n, 3),
    month    = sample(1:12, n, TRUE),
    temp_c   = round(rnorm(n, 2, 3), 1)      # legitimately includes negatives
  )
}

test_that("non-negative skewed variables stay non-negative", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE), add = TRUE)
  real <- make_catch_data()
  fp  <- fingerprint(real, mode = "summary", obfuscation = "none")
  sim <- suppressMessages(simulate_from_fingerprint(fp, n = nrow(real)))

  expect_true(all(sim$catch_kg >= 0))
  expect_true(all(sim$hooks >= 0))
  expect_true(is.integer(sim$hooks))
  expect_true(any(sim$temp_c < 0))   # genuine negatives are not floored
})

test_that("bounded integers stay within their observed range", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE), add = TRUE)
  real <- make_catch_data()
  fp  <- fingerprint(real, mode = "summary", obfuscation = "none")
  sim <- suppressMessages(simulate_from_fingerprint(fp, n = nrow(real)))
  expect_true(all(sim$month >= 1 & sim$month <= 12))
})

test_that("zero-inflation survives the round trip", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE), add = TRUE)
  real <- make_catch_data()
  fp  <- fingerprint(real, mode = "summary", obfuscation = "none")
  sim <- suppressMessages(simulate_from_fingerprint(fp, n = nrow(real)))

  expect_equal(fp$summary$catch_kg$stats$p_zero, round(mean(real$catch_kg == 0), 3))
  expect_lt(abs(mean(sim$catch_kg == 0) - mean(real$catch_kg == 0)), 0.03)
  # integer rounding must not inflate the zero mass
  expect_lt(abs(mean(sim$hooks == 0) - mean(real$hooks == 0)), 0.02)
})

test_that("p_zero is omitted for columns without zeros", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE), add = TRUE)
  fp <- fingerprint(make_catch_data(), mode = "summary", obfuscation = "none")
  expect_null(fp$summary$month$stats$p_zero)
})

test_that("fingerprints created before p_zero existed still simulate safely", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE), add = TRUE)
  fp <- fingerprint(make_catch_data(), mode = "summary", obfuscation = "none")
  fp$summary$catch_kg$stats$p_zero <- NULL
  sim <- suppressMessages(simulate_from_fingerprint(fp, n = 500))
  expect_true(all(sim$catch_kg >= 0))
})

test_that("min/max fingerprints (suppress_extremes = FALSE) stay within range", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE), add = TRUE)
  real <- make_catch_data()
  fp <- fingerprint(real, mode = "summary", obfuscation = "none",
                    anonymise = list(suppress_extremes = FALSE))
  sim <- suppressMessages(simulate_from_fingerprint(fp, n = 2000))
  expect_true(all(sim$catch_kg >= 0 & sim$catch_kg <= max(real$catch_kg)))
  expect_true(all(sim$month >= 1 & sim$month <= 12))
})
