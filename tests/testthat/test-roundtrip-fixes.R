# Regression tests for the 2026-08 QA findings: fingerprint -> simulate
# round-trip fidelity fixes, the notebook-scan per-line fix (including the
# markdown-heading false-positive guard), and the restore_names() helper.
# Each round-trip/scan test failed against v0.1.0 behaviour.

test_that("logical columns preserve their true proportion through the round trip", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE))
  set.seed(101)
  dat <- data.frame(x = rnorm(1000), keep = runif(1000) < 0.9)
  fp  <- fingerprint(dat, mode = "summary")
  expect_equal(fp$summary[["keep"]]$stats$p_true, 0.9, tolerance = 0.03)
  sim <- suppressMessages(simulate_from_fingerprint(fp))
  expect_equal(mean(sim$keep), 0.9, tolerance = 0.05)
})

test_that("date_precision = 'year' round trip stays within the source year range", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE))
  set.seed(102)
  dat <- data.frame(
    obs_date = as.Date("2016-06-01") + sample(0:700, 300, replace = TRUE),
    y = rnorm(300)
  )
  fp  <- fingerprint(dat, mode = "summary",
                     anonymise = list(date_precision = "year"))
  sim <- suppressMessages(simulate_from_fingerprint(fp))
  yrs <- as.integer(format(sim$obs_date, "%Y"))
  expect_gte(min(yrs), 2016)
  expect_lte(max(yrs), 2018)
})

test_that("day- and month-precision date round trips still work", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE))
  set.seed(103)
  dat <- data.frame(
    obs_date = as.Date("2019-03-10") + sample(0:400, 200, replace = TRUE),
    y = rnorm(200)
  )
  for (prec in c("day", "month")) {
    fp  <- fingerprint(dat, mode = "summary",
                       anonymise = list(date_precision = prec))
    sim <- suppressMessages(simulate_from_fingerprint(fp))
    expect_gte(min(sim$obs_date), as.Date("2019-02-01"))
    expect_lte(max(sim$obs_date), as.Date("2020-06-01"))
  }
})

test_that("rare missingness (0.4%) survives the round trip", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE))
  set.seed(104)
  dat <- data.frame(x = rnorm(1000), z = rnorm(1000))
  dat$x[sample(1000, 4)] <- NA
  fp <- fingerprint(dat, mode = "summary")
  expect_equal(fp$columns$p_miss[fp$columns$alias == "x"], 0.004)
  sim <- suppressMessages(simulate_from_fingerprint(fp))
  expect_gt(sum(is.na(sim$x)), 0)
})

test_that("check_notebook_outputs detects cached chunk output anywhere in the file", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---", "title: t", "---",
    "```{r}", "head(catch_data)", "```",
    "## [1] 42.7 13.1 8.9"
  ), f)
  res <- check_notebook_outputs(f, verbose = FALSE)
  expect_gt(length(res$findings), 0)
  expect_identical(res$status, "AMBER")
})

test_that("check_notebook_outputs detects tibble and aligned data-frame output", {
  for (marker in c("## # A tibble: 6 x 3",
                   "##   mpg cyl disp",
                   "## 'data.frame':\t32 obs. of  11 variables:")) {
    f <- tempfile(fileext = ".Rmd")
    writeLines(c("---", "title: t", "---", marker), f)
    res <- check_notebook_outputs(f, verbose = FALSE)
    expect_identical(res$status, "AMBER")
    unlink(f)
  }
})

test_that("check_notebook_outputs does not flag ordinary markdown headings", {
  f <- tempfile(fileext = ".Rmd")
  on.exit(unlink(f))
  writeLines(c(
    "---", "title: t", "---",
    "## Methods",
    "Some prose about the analysis.",
    "```{r}", "# fit the model", "m <- lm(y ~ x, dat)", "```",
    "## Results",
    "More prose."
  ), f)
  res <- check_notebook_outputs(f, verbose = FALSE)
  expect_identical(res$status, "GREEN")
  expect_length(res$findings, 0)
})

test_that("restore_names maps obfuscated simulated columns back to originals", {
  confidential_mode_on(verbose = FALSE)
  on.exit(confidential_mode_off(verbose = FALSE))
  set.seed(105)
  dat <- data.frame(
    vessel_name = sample(paste("FV", LETTERS[1:6]), 200, replace = TRUE),
    lat  = runif(200, -44, -32),
    catch = rlnorm(200, log(100), 0.5),
    stringsAsFactors = FALSE
  )
  fp  <- fingerprint(dat, mode = "summary", obfuscation = "partial")
  sim <- suppressMessages(simulate_from_fingerprint(fp))
  expect_true(any(grepl("^(ID|Coord)_", names(sim))))
  expect_message(fixed <- restore_names(sim, fp), "out of AI prompts")
  expect_setequal(names(fixed), names(dat))
  expect_silent(restore_names(sim, fp, verbose = FALSE))
})
