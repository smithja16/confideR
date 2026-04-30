############################################################
# safecatch – Leak detection and utilities
#
# Provides functions to detect whether an R object contains
# raw data that should not leave a confidential session.
# Covers data frames, matrices, fitted models (which embed
# their training data), and large atomic vectors.
############################################################

#' Check whether an object contains data-like content
#'
#' Returns \code{TRUE} if the object (or any nested element) looks
#' like it contains raw data that should not be shared externally.
#' This includes data frames, matrices, fitted model objects (which
#' embed raw data internally), and large atomic vectors.
#'
#' Objects of class \code{safecatch_fingerprint} are always safe.
#'
#' @param x Object to inspect.
#' @param max_vector_length Atomic vectors longer than this are
#'   flagged. Default 500.
#' @return Logical scalar.
#' @export
#' @examples
#' contains_data_like(mtcars)          # TRUE
#' contains_data_like(lm(mpg ~ wt, mtcars))  # TRUE (model embeds data)
#' contains_data_like("hello")         # FALSE
#' contains_data_like(1:100)           # FALSE (under threshold)
contains_data_like <- function(x, max_vector_length = 500) {
  !is.null(.find_leakage_reason(x, max_vector_length = max_vector_length))
}

#' Guard against data leakage
#'
#' Inspects an object and raises an informative error if it
#' contains anything that looks like raw data. Use before
#' passing objects to any function that might transmit data
#' externally.
#'
#' @param x Object to inspect.
#' @param name Label for error messages. Default \code{"object"}.
#' @param max_vector_length Threshold for large vectors. Default 500.
#' @return Invisible \code{TRUE} if safe.
#' @export
ensure_no_data_leakage <- function(x, name = "object", max_vector_length = 500) {
  reason <- .find_leakage_reason(x, max_vector_length = max_vector_length)
  if (!is.null(reason)) {
    stop(sprintf(
      paste0(
        "[safecatch] Potential data leakage in '%s'.\n",
        "  Reason: %s\n",
        "  Fix:    Only pass fingerprint objects or simple text to external tools,\n",
        "          not raw data frames, fitted models, or large vectors."
      ),
      name, reason
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# ============================================================
# Internal leak detection
# ============================================================

.find_leakage_reason <- function(x, path = "object", max_vector_length = 500) {
  # safecatch objects are always safe
  if (inherits(x, "safecatch_fingerprint")) return(NULL)

  # Data frames / tibbles / data.tables
  if (is.data.frame(x) || inherits(x, c("tbl", "tbl_df", "data.table"))) {
    return(sprintf(
      "Data frame at '%s' (%d rows x %d cols).",
      path, nrow(x), ncol(x)
    ))
  }

  # Matrices
  if (is.matrix(x)) {
    return(sprintf("Matrix at '%s' (%d x %d).", path, nrow(x), ncol(x)))
  }

  # Fitted model objects (these embed raw data)
  model_classes <- c(
    "lm", "glm", "nls",
    "gam", "gamm", "bam",
    "glmmTMB", "sdmTMB",
    "lme", "lmerMod", "glmerMod", "nlmerMod",
    "brmsfit", "stanfit", "stanreg",
    "MixMod",     # GLMMadaptive
    "RTMB",       # RTMB models
    "survreg", "coxph",
    "multinom", "polr"
  )
  if (inherits(x, model_classes)) {
    return(sprintf(
      "Fitted model (class: %s) at '%s'. Models embed raw data internally.",
      paste(class(x), collapse = "/"), path
    ))
  }

  # Lists: check field names, then recurse
  if (is.list(x) && !is.data.frame(x)) {
    suspicious_names <- c(
      "data", "data_used", "raw_data",
      "fit", "fits", "model", "model_frame", "mf",
      "residuals", "fitted.values", "X", "y", "response",
      "frame"
    )
    hits <- intersect(names(x), suspicious_names)
    if (length(hits)) {
      return(sprintf(
        "List at '%s' has suspicious fields: %s",
        path, paste(hits, collapse = ", ")
      ))
    }
    for (nm in names(x)) {
      reason <- .find_leakage_reason(x[[nm]], paste0(path, "$", nm), max_vector_length)
      if (!is.null(reason)) return(reason)
    }
  }

  # Large atomic vectors (proxy for data columns)
  if (is.atomic(x) && length(x) > max_vector_length) {
    return(sprintf(
      "Large vector (length %d) at '%s'.", length(x), path
    ))
  }

  NULL
}
