test_that("confidential mode toggles correctly", {
  confidential_mode_off(verbose = FALSE)
  expect_false(is_confidential_mode())

  confidential_mode_on(verbose = FALSE)
  expect_true(is_confidential_mode())

  confidential_mode_off(verbose = FALSE)
  expect_false(is_confidential_mode())
})

test_that("require_confidential_mode errors when off", {
  confidential_mode_off(verbose = FALSE)
  expect_error(require_confidential_mode(), "not active")
})

test_that("prevent_ai_in_confidential_mode blocks when on", {
  confidential_mode_on(verbose = FALSE)
  expect_error(prevent_ai_in_confidential_mode(), "blocked")
  confidential_mode_off(verbose = FALSE)
})

test_that("contains_data_like detects data frames", {
  expect_true(contains_data_like(mtcars))
  expect_true(contains_data_like(data.frame(x = 1:10)))
})

test_that("contains_data_like detects models", {
  m <- lm(mpg ~ wt, data = mtcars)
  expect_true(contains_data_like(m))
})

test_that("contains_data_like passes safe objects", {
  expect_false(contains_data_like("hello"))
  expect_false(contains_data_like(42))
  expect_false(contains_data_like(1:100))
  expect_false(contains_data_like(list(a = 1, b = "text")))
})

test_that("contains_data_like flags large vectors", {
  expect_true(contains_data_like(rnorm(1000)))
  expect_false(contains_data_like(rnorm(100)))
})

test_that("simulate_data produces correct structure", {
  confidential_mode_on(verbose = FALSE)
  dat <- simulate_data(n_obs = 100, n_groups = 5, seed = 1)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 100)
  expect_true(all(c("obs_id", "group_id", "year", "month", "response", "effort", "rate") %in% names(dat)))
  expect_true(all(grepl("^Group_", dat$group_id)))
  expect_true(all(grepl("^OBS_", dat$obs_id)))
  confidential_mode_off(verbose = FALSE)
})

test_that("simulate_data with optional columns", {
  confidential_mode_on(verbose = FALSE)
  dat <- simulate_data(
    n_obs = 50, n_groups = 3,
    include_observer = TRUE, include_secondary = TRUE, include_spatial = TRUE,
    seed = 1
  )
  expect_true("observer_id" %in% names(dat))
  expect_true("secondary_event" %in% names(dat))
  expect_true("latitude" %in% names(dat))
  expect_true("longitude" %in% names(dat))
  confidential_mode_off(verbose = FALSE)
})

test_that("fingerprint produces safe output", {
  confidential_mode_on(verbose = FALSE)
  dat <- simulate_data(n_obs = 100, n_groups = 5, seed = 1)
  fp <- fingerprint(dat, mode = "summary", obfuscation = "partial")
  expect_s3_class(fp, "confider_fingerprint")
  expect_false(contains_data_like(fp))
  expect_equal(fp$meta$n_rows, 100)
  confidential_mode_off(verbose = FALSE)
})

test_that("fingerprint obfuscation levels work", {
  confidential_mode_on(verbose = FALSE)
  dat <- simulate_data(n_obs = 50, n_groups = 3, seed = 1)

  fp_none    <- fingerprint(dat, obfuscation = "none")
  fp_partial <- fingerprint(dat, obfuscation = "partial")
  fp_full    <- fingerprint(dat, obfuscation = "full")

  # None: original names kept
  expect_true("group_id" %in% fp_none$columns$alias)
  # Partial: group_id should be aliased (it's a high-cardinality identifier)
  expect_true(any(grepl("^ID_", fp_partial$columns$alias)))
  # Full: all aliased
  expect_true(all(grepl("^Var", fp_full$columns$alias)))

  confidential_mode_off(verbose = FALSE)
})

test_that("simulate_from_fingerprint matches structure", {
  confidential_mode_on(verbose = FALSE)
  dat <- simulate_data(n_obs = 100, n_groups = 5, seed = 1)
  fp <- fingerprint(dat, mode = "summary", obfuscation = "none")
  sim <- simulate_from_fingerprint(fp, n = 50, seed = 99)
  expect_equal(ncol(sim), fp$meta$n_cols)
  expect_equal(nrow(sim), 50)
  confidential_mode_off(verbose = FALSE)
})

test_that("audit functions run without error", {
  expect_no_error(audit_ide(verbose = FALSE))
  expect_no_error(audit_packages(verbose = FALSE))
  expect_no_error(audit_env_keys(verbose = FALSE))
  expect_no_error(audit_processes(verbose = FALSE))
})

test_that("audit_env_keys returns expected structure", {
  r <- audit_env_keys(verbose = FALSE)
  expect_true(all(c("found_live", "found_on_disk", "status") %in% names(r)))
})

test_that("audit_processes returns expected structure", {
  r <- audit_processes(verbose = FALSE)
  expect_true(all(c("found", "status") %in% names(r)))
  expect_true(r$status %in% c("GREEN", "AMBER"))
})

test_that("scan_script detects common patterns", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "# Analysis of catch_data from FV Doris Bay",
    "dat <- read.csv('/Users/jsmith/Projects/secret/data.csv')",
    "contact <- 'john.smith@example.com'",
    "coords <- c(-46.5231, 168.3456)",
    "print(head(dat))",
    "write_clip(dat)"
  ), tmp)

  r <- scan_script(tmp, verbose = FALSE)
  expect_true(length(r$findings) >= 5)
  expect_equal(r$status, "AMBER")

  patterns_found <- unique(vapply(r$findings, function(f) f$pattern, character(1)))
  expect_true("vessel_identifier" %in% patterns_found)
  expect_true("absolute_user_path" %in% patterns_found)
  expect_true("email_address" %in% patterns_found)

  unlink(tmp)
})

test_that("scan_script returns GREEN on clean scripts", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c(
    "library(dplyr)",
    "dat <- read.csv('./data/survey.csv')",
    "model <- lm(log_rate ~ year + group_id, data = dat)",
    "summary(model)"
  ), tmp)

  r <- scan_script(tmp, verbose = FALSE)
  patterns_found <- vapply(r$findings, function(f) f$pattern, character(1))
  expect_false("vessel_identifier" %in% patterns_found)
  expect_false("email_address" %in% patterns_found)
  expect_false("absolute_user_path" %in% patterns_found)

  unlink(tmp)
})

test_that("check_notebook_outputs handles missing file gracefully", {
  expect_error(check_notebook_outputs("/does/not/exist.Rmd"), "not found")
})
