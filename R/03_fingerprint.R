############################################################
# confideR – Data fingerprinting
#
# Extracts a privacy-safe structural summary of a confidential
# data frame: column names, types, missingness, and optionally
# sanitised summary statistics. No raw values are stored.
#
# Obfuscation levels control column name exposure:
#   "none"    – original names kept
#   "partial" – confidential columns aliased; others kept
#   "full"    – all columns aliased (Var1, Var2, ...)
############################################################

#' Create a privacy-safe fingerprint of a confidential dataset
#'
#' Summarises structure and statistics without retaining raw values.
#' The result is safe to share with AI tools for code generation.
#'
#' @param data A data frame.
#' @param mode \code{"names_and_types"} (safest) or \code{"summary"}
#'   (adds sanitised summary statistics).
#' @param obfuscation \code{"none"}, \code{"partial"}, or \code{"full"}.
#' @param confidential Optional named list with \code{identifiers},
#'   \code{coords}, and \code{other} character vectors of column names
#'   to treat as confidential. If \code{NULL}, auto-detected.
#' @param anonymise List of thresholds: \code{numeric_digits},
#'   \code{min_level_size}, \code{max_levels_shown}, \code{date_precision}.
#' @return An object of class \code{confider_fingerprint}.
#' @export
fingerprint <- function(
    data,
    mode        = c("names_and_types", "summary"),
    obfuscation = c("none", "partial", "full"),
    confidential = NULL,
    anonymise   = list(
      numeric_digits   = 2,
      min_level_size   = 5,
      max_levels_shown = 20,
      date_precision   = "month"
    )
) {
  stopifnot(is.data.frame(data))
  require_confidential_mode("Fingerprinting requires confidential mode.")

  mode        <- match.arg(mode)
  obfuscation <- match.arg(obfuscation)

  # Defaults
  anonymise$numeric_digits   <- anonymise$numeric_digits   %||% 2
  anonymise$min_level_size   <- anonymise$min_level_size   %||% 5
  anonymise$max_levels_shown <- anonymise$max_levels_shown %||% 20
  anonymise$date_precision   <- anonymise$date_precision   %||% "month"

  # 1. Semantic types
  col_types <- vapply(data, .semantic_type, character(1))

  # 2. Auto-detect confidential columns
  if (is.null(confidential)) {
    confidential <- .detect_confidential_cols(data, col_types)
  }
  all_conf <- unique(unlist(confidential))

  # 3. Alias map
  alias_map <- .build_alias_map(names(data), all_conf, col_types, obfuscation)

  # 4. Column info
  col_df <- data.frame(
    alias    = unname(alias_map),
    type     = unname(col_types),
    n_miss   = vapply(data, function(x) sum(is.na(x)), integer(1)),
    # Round to 2dp to avoid disclosing exact missing counts via proportion
    p_miss   = round(vapply(data, function(x) mean(is.na(x)), numeric(1)), 2),
    confidential = names(data) %in% all_conf,
    stringsAsFactors = FALSE
  )

  out <- structure(
    list(
      meta = list(
        n_rows      = nrow(data),
        n_cols      = ncol(data),
        mode        = mode,
        obfuscation = obfuscation,
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      columns        = col_df,
      confidential   = confidential,
      .alias_map     = alias_map    # internal - never sent to AI
    ),
    class = "confider_fingerprint"
  )

  # 5. Summary statistics
  if (mode == "summary") {
    out$summary <- .compute_column_summaries(data, col_types, all_conf, alias_map, anonymise)
  }

  out
}

#' Test whether an object is a confideR fingerprint
#' @param x Any R object.
#' @return Logical.
#' @export
is_fingerprint <- function(x) inherits(x, "confider_fingerprint")

#' Retrieve the alias-to-original-name mapping
#'
#' For in-session reference only. Never include in prompts or payloads.
#'
#' @param fp A \code{confider_fingerprint} object.
#' @return Data frame with columns \code{original} and \code{alias}.
#' @export
alias_map <- function(fp) {
  stopifnot(is_fingerprint(fp))
  data.frame(
    original = names(fp$.alias_map),
    alias    = unname(fp$.alias_map),
    stringsAsFactors = FALSE
  )
}

#' Format a fingerprint as text for pasting into an AI chat
#'
#' Produces a plain-text description of the dataset structure suitable
#' for copy-pasting into a browser AI chat (Scenario 1). No raw values
#' are included.
#'
#' @param fp A \code{confider_fingerprint} object.
#' @return Character string (invisibly). Also prints to console.
#' @export
format_for_prompt <- function(fp) {
  stopifnot(is_fingerprint(fp))

  lines <- c(
    "## Dataset structural summary (confideR fingerprint)",
    sprintf("Approximate size: %d rows x %d columns", fp$meta$n_rows, fp$meta$n_cols),
    ""
  )

  for (i in seq_len(nrow(fp$columns))) {
    r <- fp$columns[i, ]
    # Report proportion missing, not exact count — counts are disclosive
    miss <- if (r$p_miss > 0) sprintf(" [~%.0f%% missing]", r$p_miss * 100) else ""
    lines <- c(lines, sprintf("- %s (%s)%s", r$alias, r$type, miss))

    if (!is.null(fp$summary)) {
      s <- fp$summary[[r$alias]]
      if (!is.null(s$stats)) {
        stats_nn <- Filter(Negate(is.null), s$stats)
        stat_strs <- vapply(names(stats_nn), function(nm) {
          val <- stats_nn[[nm]]
          sprintf("%s=%s", nm, if (length(val) > 1) paste(val, collapse = "/") else val)
        }, character(1))
        lines <- c(lines, sprintf("    %s", paste(stat_strs, collapse = ", ")))
      }
    }
  }

  # --- Safety footer (instruction to the AI) ---
  # Included so the model is explicitly told not to elicit raw data from
  # the user. This is a soft defence — the model is not obligated to
  # follow it — but in practice it reduces the likelihood of the model
  # asking for sample rows that would trigger the user to paste data.
  lines <- c(lines,
    "",
    "## Instructions for the AI",
    "- This is a structural summary only. No raw data rows or individual values are included.",
    "- Do not ask me to share raw data, sample rows, or specific values from this dataset.",
    "- Reason from the structure (column names, types, distributions, missingness) to write code.",
    "- If you need clarification, ask about the structure rather than the contents."
  )

  txt <- paste(lines, collapse = "\n")
  cat(txt, "\n")
  invisible(txt)
}

# ============================================================
# S3 methods
# ============================================================

#' @export
print.confider_fingerprint <- function(x, ...) {
  cat("-- confideR fingerprint --\n")
  cat(sprintf("  Rows: %d   Cols: %d\n", x$meta$n_rows, x$meta$n_cols))
  cat(sprintf("  Mode: %s   Obfuscation: %s\n", x$meta$mode, x$meta$obfuscation))
  n_conf <- sum(x$columns$confidential)
  if (n_conf > 0) cat(sprintf("  Confidential columns: %d\n", n_conf))
  cat("\n  Columns:\n")
  for (i in seq_len(nrow(x$columns))) {
    r <- x$columns[i, ]
    miss <- if (r$n_miss > 0) sprintf(" [%d NA]", r$n_miss) else ""
    conf <- if (r$confidential) " *conf*" else ""
    cat(sprintf("    %-25s %s%s%s\n", r$alias, r$type, miss, conf))
  }
  invisible(x)
}

# ============================================================
# Internal helpers
# ============================================================

.semantic_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
  if (is.logical(x))   return("logical")
  if (is.numeric(x))   return(if (all(x %% 1 == 0, na.rm = TRUE)) "integer" else "continuous")
  if (is.factor(x) || is.character(x)) return("categorical")
  "other"
}

# Auto-detection covers fisheries, ecological, environmental,
# agricultural, and social/health science naming conventions.
# Pass an explicit `confidential` list to fingerprint() to override.
.detect_confidential_cols <- function(data, col_types) {
  nm <- names(data)
  nm_l <- tolower(nm)
  n <- nrow(data)

  # Identifier patterns — covers fisheries, ecological, environmental,
  # agricultural, and social/health science column naming conventions.
  id_pat <- paste0(
    "\\bid\\b|_id$|^id_|^id$",
    "|name$|_name$|^name_",
    "|phone|email|address|postcode|zipcode|dob|birthdate",
    "|ssn|passport|license|licence|permit|rego|callsign",
    # fisheries
    "|skipper|vessel|boat|fisher|captain|master|hull",
    # ecological / environmental
    "|observer|crew|site|plot|transect|station|quadrat|trap|nest",
    # agricultural
    "|farm|grower|landowner|paddock|property",
    # social / health
    "|client|patient|participant|respondent|household|informant|interviewee|donor|recipient",
    "|registration"
  )
  id_by_name <- nm[grepl(id_pat, nm_l)]

  # High-cardinality character columns
  id_by_card <- nm[vapply(seq_along(nm), function(i) {
    col_types[i] == "categorical" && (length(unique(data[[nm[i]]])) / n) > 0.4
  }, logical(1))]

  # Coordinate patterns
  coord_pat <- "^lon$|^long$|longitude|x_coord|easting|utm_e|^lat$|latitude|y_coord|northing|utm_n"
  coords <- nm[grepl(coord_pat, nm_l)]

  list(
    identifiers = unique(c(id_by_name, id_by_card)),
    coords      = coords,
    other       = character(0)
  )
}

.build_alias_map <- function(col_names, conf_cols, col_types, obfuscation) {
  aliases <- col_names
  if (obfuscation == "full") {
    aliases <- paste0("Var", seq_along(col_names))
  } else if (obfuscation == "partial") {
    id_ctr <- 0L; coord_ctr <- 0L
    for (i in seq_along(col_names)) {
      if (col_names[i] %in% conf_cols) {
        if (grepl("lon|lat|coord|easting|northing|utm", tolower(col_names[i]))) {
          coord_ctr <- coord_ctr + 1L
          aliases[i] <- paste0("Coord_", coord_ctr)
        } else {
          id_ctr <- id_ctr + 1L
          aliases[i] <- paste0("ID_", id_ctr)
        }
      }
    }
  }
  names(aliases) <- col_names
  aliases
}

.compute_column_summaries <- function(data, col_types, conf_cols, alias_map, anon) {
  # Defaults for new safety parameters
  suppress_extremes <- anon$suppress_extremes %||% TRUE
  extreme_neighborhood <- anon$extreme_neighborhood %||% 3L

  result <- list()
  for (i in seq_along(names(data))) {
    alias <- unname(alias_map[i])
    x     <- data[[names(data)[i]]]
    type  <- col_types[i]
    is_conf <- names(data)[i] %in% conf_cols

    stats <- switch(type,
      continuous = , integer = {
        xf <- x[is.finite(x)]
        if (length(xf) == 0) list(note = "all missing") else {
          # Suppress min/max for confidential columns or when requested —
          # single extreme values can identify individual records.
          # Report 5th/95th percentiles as a safer alternative.
          if (suppress_extremes || is_conf) {
            probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
            labels <- c("q05", "q25", "median", "q75", "q95")
          } else {
            probs <- c(0, 0.25, 0.5, 0.75, 1)
            labels <- c("min", "q25", "median", "q75", "max")
          }
          qs <- stats::quantile(xf, probs, names = FALSE)
          out <- as.list(round(qs, anon$numeric_digits))
          names(out) <- labels
          out$mean <- round(mean(xf), anon$numeric_digits)
          out$sd   <- round(stats::sd(xf), anon$numeric_digits)
          out$n    <- length(xf)
          out
        }
      },
      categorical = {
        xc <- x[!is.na(x)]
        tab <- sort(table(xc), decreasing = TRUE)
        # Only list levels if: few enough total, all above min_level_size,
        # AND the column is not flagged as confidential.
        safe <- length(tab) <= anon$max_levels_shown &&
                all(tab >= anon$min_level_size) &&
                !is_conf
        list(n_levels = length(tab),
             levels = if (safe) names(tab) else NULL,
             note = if (!safe) sprintf("%d levels (names suppressed)", length(tab)) else NULL)
      },
      date = {
        xd <- x[!is.na(x)]
        fmt <- switch(anon$date_precision, day = "%Y-%m-%d", month = "%Y-%m", year = "%Y", "%Y-%m")
        if (length(xd)) list(min = format(min(xd), fmt), max = format(max(xd), fmt), n = length(xd))
        else list(note = "all missing")
      },
      NULL
    )

    result[[alias]] <- list(type = type, confidential = is_conf, stats = stats)
  }
  result
}
