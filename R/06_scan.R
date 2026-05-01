############################################################
# confideR – Script and notebook scanner
#
# Scans R scripts, R Markdown, and Quarto files for content
# that may accidentally expose confidential data when shared
# with AI tools. Flags hardcoded values, suspicious file
# paths, descriptive comments, and rendered notebook outputs.
#
# Designed for the workflow:
#   scan_script("my_analysis.R")
#   # review findings, manually redact or regenerate
#   scan_script("my_analysis.R")  # verify clean
############################################################

# --- Patterns that may indicate embedded confidential data -------------

# Likely real data fragments in string literals or hard-coded values.
# These are conservative — false positives are expected and should be
# reviewed by the user.
.confider_scan_patterns <- list(

  list(
    name = "absolute_user_path",
    pattern = "/(Users|home)/[A-Za-z0-9._-]+/",
    advice = "Absolute path reveals your username and directory structure. Use relative paths (./data/file.csv) or here::here()."
  ),

  list(
    name = "windows_user_path",
    pattern = "[Cc]:[/\\\\]+(Users|Documents)[/\\\\]",
    advice = "Absolute Windows path reveals user directory. Use relative paths or here::here()."
  ),

  list(
    name = "email_address",
    pattern = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
    advice = "Email address in code may identify a real person."
  ),

  list(
    name = "phone_like",
    # Various phone number formats; deliberately loose.
    pattern = "\\b(\\+?\\d{1,3}[\\s.-]?)?\\(?\\d{3,4}\\)?[\\s.-]?\\d{3,4}[\\s.-]?\\d{3,4}\\b",
    advice = "Phone-number-like pattern detected. If real, redact before sharing."
  ),

  list(
    name = "coordinate_pair",
    # Decimal coords e.g. -46.5231, 168.3456 or lat=-46.5 lon=168
    pattern = "-?\\d{1,3}\\.\\d{3,}\\s*[,;]\\s*-?\\d{1,3}\\.\\d{3,}",
    advice = "Precise coordinate pair detected. Jitter, snap to grid, or replace with statistical area before sharing."
  ),

  list(
    name = "fisheries_vessel_pattern",
    # Common vessel-name patterns: FV Name, MV Name, F/V Name
    pattern = "\\b(FV|MV|F/V|MFV)\\s+[A-Z][A-Za-z0-9]+",
    advice = "Vessel name detected (FV/MV prefix). Replace with anonymised vessel ID."
  ),

  list(
    name = "permit_like_id",
    # Patterns like FIS-2024-0892, PERMIT_XYZ_001
    pattern = "\\b(FIS|PERMIT|OBS|LIC)[-_][A-Z0-9]+[-_][A-Z0-9]+\\b",
    advice = "Identifier pattern detected (possible permit/licence number). Anonymise before sharing."
  ),

  list(
    name = "descriptive_comment",
    # Comments that mention real-sounding contexts
    pattern = "#\\s*.*\\b(vessel|skipper|observer|fisher|fishery|reserve|permit|licence|license|client|patient|subject)\\b",
    advice = "Comment references a domain-specific entity. Check whether it names a real vessel, person, or location."
  ),

  list(
    name = "hardcoded_data_in_vector",
    # Vectors of many real-looking strings: c("J Smith", "M Brown", ...)
    pattern = "c\\s*\\(\\s*\"[A-Z][a-z]+\\s+[A-Z][a-zA-Z]+\"(\\s*,\\s*\"[A-Z][a-z]+\\s+[A-Z][a-zA-Z]+\"){2,}",
    advice = "Vector of name-like strings detected. If these are real names, redact before sharing."
  ),

  list(
    name = "View_call",
    # View() displays data — if the script is shared while the viewer was active,
    # the rendered output may accompany it.
    pattern = "\\bView\\s*\\(",
    advice = "View() call — consider removing before sharing; the viewer output may display real data."
  ),

  list(
    name = "print_data_object",
    # print(df) or head(df) calls — likely to render data to console
    pattern = "\\b(print|head|tail|str|summary|glimpse)\\s*\\(\\s*[a-zA-Z_][a-zA-Z0-9_.]*\\s*[,)]",
    advice = "Data inspection call (print/head/str/etc). Output may appear in rendered notebook or pasted error output."
  ),

  list(
    name = "clipboard_write",
    # Clipboard writes can move data to chat apps when pasted
    pattern = "\\b(write_clip|writeClipboard|clipr::write_clip|write.table\\(.*clipboard)",
    advice = "Clipboard write detected. If the clipboard contents are pasted into a browser AI chat, data will leak."
  ),

  list(
    name = "save_to_disk",
    # saveRDS / save — fitted models embed training data
    pattern = "\\b(save|saveRDS|write_rds|write.csv|write_csv)\\s*\\(",
    advice = "File write detected. If this saves a model object or real data, it may be shared accidentally via Git or file transfer."
  )
)


#' Scan an R script, R Markdown, or Quarto file for confidentiality risks
#'
#' Reads the file and searches each line for patterns that commonly
#' indicate embedded real data or references to confidential entities.
#' Useful before pasting a script into an AI chat or committing it
#' to a shared repository.
#'
#' @param path Path to a .R, .Rmd, .qmd, or .Rnw file.
#' @param patterns Optional character vector of pattern names to include
#'   (see \code{.confider_scan_patterns}). Default \code{NULL} uses all.
#' @param verbose Print findings? Default \code{TRUE}.
#' @return Invisibly, a list with \code{findings} (list of line-level
#'   matches) and \code{status} (\code{"GREEN"} or \code{"AMBER"}).
#' @export
#' @examples
#' \dontrun{
#'   scan_script("analysis/cpue_model.R")
#' }
scan_script <- function(path, patterns = NULL, verbose = TRUE) {
  if (!file.exists(path)) {
    stop("[confideR] File not found: ", path, call. = FALSE)
  }

  # --- Read file ---
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) {
    stop("[confideR] Could not read file: ", conditionMessage(e), call. = FALSE)
  })

  # --- Select patterns ---
  active_patterns <- if (is.null(patterns)) {
    .confider_scan_patterns
  } else {
    Filter(function(p) p$name %in% patterns, .confider_scan_patterns)
  }

  # --- Scan each line ---
  findings <- list()
  for (i in seq_along(lines)) {
    line <- lines[i]
    # Skip empty lines
    if (!nzchar(trimws(line))) next

    for (p in active_patterns) {
      if (grepl(p$pattern, line, perl = TRUE)) {
        findings <- c(findings, list(list(
          line_number = i,
          line_text   = trimws(line),
          pattern     = p$name,
          advice      = p$advice
        )))
      }
    }
  }

  status <- if (length(findings) > 0) "AMBER" else "GREEN"

  result <- list(
    path     = path,
    findings = findings,
    status   = status,
    n_lines  = length(lines)
  )

  # --- Report ---
  if (verbose) {
    cat("\n")
    cat(sprintf("=== confideR: scan of %s ===\n", basename(path)))
    cat(sprintf("  Lines scanned: %d\n", length(lines)))
    cat(sprintf("  Findings:      %d\n\n", length(findings)))

    if (length(findings) == 0) {
      cat("  No suspicious patterns detected by automated checks.\n")
      cat("  Manual review is still recommended before sharing externally.\n\n")
    } else {
      # Group findings by pattern
      pattern_counts <- table(vapply(findings, function(f) f$pattern, character(1)))
      cat("  Finding summary:\n")
      for (nm in names(pattern_counts)) {
        cat(sprintf("    %-30s %d match(es)\n", nm, pattern_counts[nm]))
      }
      cat("\n  Details:\n")
      for (f in findings) {
        cat(sprintf("    Line %d [%s]:\n", f$line_number, f$pattern))
        # Truncate very long lines
        disp <- if (nchar(f$line_text) > 100) paste0(substr(f$line_text, 1, 97), "...") else f$line_text
        cat(sprintf("      > %s\n", disp))
        cat(sprintf("      Advice: %s\n\n", f$advice))
      }
      cat("  NOTE: These are heuristic matches. Some findings will be\n")
      cat("  false positives. Review each one before deciding to redact.\n\n")
    }
  }

  invisible(result)
}


#' Check a rendered notebook for embedded data output
#'
#' R Markdown (.Rmd), Quarto (.qmd), and Jupyter (.ipynb) files can
#' store rendered chunk output — including printed tables and plots —
#' directly in the file. This output may contain real data values even
#' if the source code would not. This function looks for signs of
#' rendered output in the file.
#'
#' @param path Path to a .Rmd, .qmd, .ipynb, or rendered .html file.
#' @param verbose Print findings? Default \code{TRUE}.
#' @return Invisibly, a list with \code{findings} and \code{status}.
#' @export
check_notebook_outputs <- function(path, verbose = TRUE) {
  if (!file.exists(path)) {
    stop("[confideR] File not found: ", path, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  content <- paste(lines, collapse = "\n")

  findings <- list()

  # --- Heuristics for each format ---

  if (ext == "ipynb") {
    # Jupyter notebooks store outputs in JSON "outputs" arrays
    if (grepl('"outputs"\\s*:\\s*\\[\\s*\\{', content)) {
      # Count non-empty output blocks
      n_outputs <- length(gregexpr('"output_type"', content)[[1]])
      if (n_outputs > 0) {
        findings <- c(findings, list(list(
          issue = sprintf("Notebook contains %d rendered output block(s).", n_outputs),
          advice = "Open in Jupyter and run 'Kernel > Restart & Clear All Outputs' before sharing."
        )))
      }
    }
  }

  if (ext %in% c("rmd", "qmd")) {
    # Signs of rendered output in markdown notebooks:
    # - HTML tables (<table>...</table>) inside chunks
    # - Base64-encoded images
    # - Printed data chunks (```{r} ...```) with output markers
    # - knitr::kable output cached in file

    if (grepl("<table[^>]*>", content)) {
      findings <- c(findings, list(list(
        issue = "Rendered HTML table(s) detected in file.",
        advice = "File may contain cached output with real data. Re-render after clearing, or work from a fresh .Rmd/.qmd source."
      )))
    }
    if (grepl("data:image/[a-z]+;base64,", content)) {
      findings <- c(findings, list(list(
        issue = "Base64-encoded image(s) embedded in file.",
        advice = "Embedded plots may contain labelled data points. Review plots for real identifiers before sharing."
      )))
    }
    # Check for chunk output markers (knitr-style)
    if (grepl("^##\\s+\\[1\\]|^##\\s+[A-Z]", content, perl = TRUE)) {
      findings <- c(findings, list(list(
        issue = "Lines starting with '## ' detected (possible cached chunk output).",
        advice = "These may be R output saved alongside the source. Clear and re-render if sharing."
      )))
    }
  }

  if (ext == "html") {
    # Rendered HTML: always warn, as it's a snapshot of output
    findings <- c(findings, list(list(
      issue = "This is a rendered HTML file — any output shown in the file was generated from real data.",
      advice = "Do not share rendered HTML generated from confidential data. Regenerate from simulated data if needed for demonstration."
    )))
  }

  status <- if (length(findings) > 0) "AMBER" else "GREEN"
  result <- list(path = path, findings = findings, status = status)

  if (verbose) {
    cat("\n")
    cat(sprintf("=== confideR: notebook check of %s ===\n", basename(path)))
    if (length(findings) == 0) {
      cat("  No rendered output detected in file.\n")
      cat("  Manual review still recommended.\n\n")
    } else {
      for (f in findings) {
        cat(sprintf("\n  ! %s\n", f$issue))
        cat(sprintf("    Advice: %s\n", f$advice))
      }
      cat("\n")
    }
  }

  invisible(result)
}


# ============================================================
# Could-haves for future versions (not yet implemented)
# ============================================================
#
# redact_script(path, output_path = NULL):
#   Auto-rewrite a script replacing flagged patterns with placeholders.
#   Would need careful handling — automatic redaction could silently
#   break code. Safer as an interactive prompt: show each match and
#   ask for replacement text.
#
# install_git_precommit_hook(repo = getwd()):
#   Write a .git/hooks/pre-commit shell script that runs scan_script()
#   on every staged .R/.Rmd/.qmd file and blocks commits when findings
#   exceed a threshold. The most effective backstop against accidentally
#   committing real data to a shared repository.
#
# monitor_clipboard(enable = TRUE):
#   Hook into clipr::write_clip / writeClipboard / utils::writeLines
#   to check contains_data_like() before the write succeeds. Blocks
#   the "copy data, paste into browser AI" pathway. Requires careful
#   implementation to avoid breaking legitimate clipboard usage.
#
# guard_save(enable = TRUE):
#   Hook into save() / saveRDS() / write_rds() to warn when the object
#   contains_data_like() and the output path is inside a git repo.
#   Catches the "saved model with embedded data then committed" case.
#
# scan_git_history(n_commits = 50):
#   Run scan_script() across recent commits to detect historical leaks.
#   Would use git log + git show to read prior versions of files.
#   Helps users discover data that has already been committed.
#
# detect_shiny_ai_backend():
#   Inspect Shiny app code for httr/httr2 calls to known AI endpoints,
#   flagging apps that may transmit user data to cloud models.
#
# differential_privacy_summary():
#   Alternative to fingerprint() summaries that adds calibrated noise
#   to statistics (Laplace mechanism) for stronger formal guarantees.
#   Relevant for very sensitive datasets where even quartiles could
#   be disclosive.
#
# reticulate_python_scan():
#   When reticulate is loaded, scan imported Python modules against a
#   blocklist (openai, anthropic, langchain, etc.). Needed because
#   the R-level AI package blocklist doesn't catch Python-side leaks.
#
# scan_staged_files():
#   Convenience wrapper that runs scan_script() on all files currently
#   staged in git (`git diff --cached --name-only`). Useful for
#   pre-commit review without installing a hook.
#
# policy_file_loader(path = "confider.yml"):
#   Load project-specific config (blocklist extensions, scan patterns,
#   fingerprint thresholds) from a YAML file. Lets institutions
#   distribute standard safety configs to their researchers.
#
# ============================================================
