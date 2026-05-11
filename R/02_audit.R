############################################################
# confideR – Session auditing
#
# Detects IDE type, active AI features, suspicious .Rprofile
# entries, loaded AI packages, and live API keys. Returns
# structured reports with green/amber/red status levels.
############################################################

#' Audit the current session for AI data exposure risks
#'
#' Runs all audit checks and prints a consolidated report with
#' colour-coded status indicators. This is the recommended
#' first step when starting work on a confidential project.
#'
#' Automated checks cover: IDE detection and known AI feature status,
#' loaded AI-adjacent R packages, AI API keys in the live environment
#' and .Renviron files, and .Rprofile entries indicating AI
#' auto-connections.
#'
#' Note that automated coverage varies by IDE. confideR has most
#' visibility in RStudio (where rstudioapi allows direct inspection
#' of settings). In Positron and VS Code, IDE-level AI
#' features (Assistant configuration, extensions) are stored in
#' settings files that R cannot read directly; manual verification
#' is more important in those environments.
#'
#' @param check_rprofile Logical. Scan .Rprofile files? Default \code{TRUE}.
#' @param verbose Logical. Print the report? Default \code{TRUE}.
#' @return Invisibly, a list with components \code{ide}, \code{packages},
#'   \code{env_keys}, \code{rprofile}, and \code{overall_status}.
#' @export
#' @examples
#' audit_session()
audit_session <- function(check_rprofile = TRUE, verbose = TRUE) {

  ide_info   <- audit_ide(verbose = FALSE)
  pkg_info   <- audit_packages(verbose = FALSE)
  key_info   <- audit_env_keys(verbose = FALSE)
  rprof_info <- if (check_rprofile) audit_rprofile(verbose = FALSE) else NULL

  # Overall status
  statuses <- c(
    ide_info$status,
    pkg_info$status,
    key_info$status,
    if (!is.null(rprof_info)) rprof_info$status
  )
  overall <- if ("RED" %in% statuses) "RED"
             else if ("AMBER" %in% statuses) "AMBER"
             else "GREEN"

  result <- list(
    ide       = ide_info,
    packages  = pkg_info,
    env_keys  = key_info,
    rprofile  = rprof_info,
    confidential_mode = is_confidential_mode(),
    overall_status    = overall
  )

  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("   confideR: Session Audit Report\n")
    cat("========================================\n\n")

    # Confidential mode
    if (is_confidential_mode()) {
      cat("  Confidential mode:  ACTIVE\n\n")
    } else {
      cat("  Confidential mode:  INACTIVE\n")
      cat("  --> Call confidential_mode_on() before loading data\n\n")
    }

    # IDE — show which IDE was detected and note coverage level
    .print_status_line("IDE", ide_info$ide_name, ide_info$status)
 
    # Report what confideR can and cannot check for this specific IDE
    if (ide_info$ide_detail$is_rstudio) {
      cat("    [i] RStudio detected. confideR can inspect Copilot settings\n")
      cat("        directly via rstudioapi.\n")
    } else if (ide_info$ide_detail$is_positron) {
      cat("    [i] Positron detected. Positron Assistant and Copilot settings\n")
      cat("        are stored in settings.json, which R cannot read directly.\n")
      cat("        Automated checks are limited to environment variables and\n")
      cat("        loaded R packages. Manual verification is essential.\n")
    } else if (ide_info$ide_detail$is_vscode) {
      cat("    [i] VS Code detected. Extension settings, Claude Code config\n")
      cat("        (~/.claude/), and Copilot tokens are stored outside R's\n")
      cat("        visibility. Automated checks cover environment variables\n")
      cat("        and R packages only. Manual verification is essential.\n")
    } else {
      cat("    [i] Terminal or unknown IDE detected. If you are using an IDE,\n")
      cat("        confideR may not have detected it correctly. Check IDE AI\n")
      cat("        settings manually before proceeding.\n")
    }
    cat("\n")
    
    for (w in ide_info$warnings) cat("    ! ", w, "\n")
    if (length(ide_info$warnings)) cat("\n")

    # Packages
    .print_status_line("AI packages loaded",
      if (length(pkg_info$loaded)) paste(pkg_info$loaded, collapse = ", ")
      else "(none)",
      pkg_info$status
    )

    # API keys
    all_keys <- c(
      key_info$found_live,
      vapply(key_info$found_on_disk, function(f) f$key, character(1))
    )
    .print_status_line("AI API keys",
      if (length(all_keys)) paste(unique(all_keys), collapse = ", ")
      else "(none)",
      key_info$status
    )

    # .Rprofile
    if (!is.null(rprof_info)) {
      .print_status_line(".Rprofile AI entries",
        if (length(rprof_info$findings)) paste(length(rprof_info$findings), "found")
        else "(none)",
        rprof_info$status
      )
      for (f in rprof_info$findings) {
        cat("    ! ", f$file, ": ", f$line_text, "\n")
      }
    }

    cat("\n  ----------------------------------------\n")
    .print_status_line("OVERALL", overall, overall)
    cat("  ----------------------------------------\n")

    if (overall == "RED") {
      cat("\n  ACTION REQUIRED: AI features detected.\n")
      cat("  Run confidential_mode_on() to clear keys and\n")
      cat("  unload AI packages before loading data.\n")
    } else if (overall == "AMBER") {
      cat("\n  CAUTION: Potential AI exposure detected.\n")
      cat("  Review the warnings above before proceeding.\n")
    } else {
      cat("\n  No AI risks detected by automated checks.\n")
    }

    # Manual checks — tailored to IDE where possible.
    # These always appear because automated checks cannot cover everything,
    # and the gaps are larger in some IDEs than others.
    cat("\n  Manual checks (confideR cannot verify these automatically):\n")
      
    # IDE-specific manual checks come first
    if (ide_info$ide_detail$is_rstudio) {
      cat("    [ ] Global Options > Copilot: confirm Copilot is disabled\n")
      cat("        (confideR checks this via rstudioapi, but verify visually\n")
      cat("        if the automated check returned AMBER or could not verify)\n")
      cat("    [ ] Addins menu: check no AI addins (gptstudio, gander, chattr)\n")
      cat("        are active or have been invoked in this session\n")
    } else if (ide_info$ide_detail$is_positron) {
      cat("    [ ] Settings > Positron Assistant: confirm the Assistant is\n")
      cat("        disabled or set to a local-only model\n")
      cat("    [ ] Settings > Extensions > GitHub Copilot: confirm Copilot\n")
      cat("        is disabled (it is enabled by default if you have a\n")
      cat("        GitHub Copilot subscription)\n")
      cat("    [ ] Check that your R console history does not show data output\n")
      cat("        from head(), print(), or summary() calls — Positron Assistant\n")
      cat("        reads console history as context\n")
    } else if (ide_info$ide_detail$is_vscode) {
      cat("    [ ] Extensions sidebar: confirm GitHub Copilot and Claude Code\n")
      cat("        extensions are disabled\n")
      cat("    [ ] If Claude Code was used previously, check ~/.claude/ for\n")
      cat("        cached session content\n")
      cat("    [ ] Check VS Code settings.json for any AI extension\n")
      cat("        configurations (Ctrl+Shift+P > 'Open User Settings JSON')\n")
    }
 
    # Universal manual checks that apply to all IDEs
    cat("    [ ] Are any data files open in editor tabs that AI\n")
    cat("        autocomplete could read?\n")
    cat("    [ ] Does your R console show printed data output\n")
    cat("        (e.g. from a previous head() or summary() call)?\n")
    cat("    [ ] Are you on a shared workstation where another user\n")
    cat("        may have configured AI tools?\n")
    cat("    [ ] Have you checked for AI-enabled R packages not on\n")
    cat("        confideR's blocklist? (The blocklist is not exhaustive)\n")
 
    # Remind VS Code and Positron users that automated coverage is partial
    if (ide_info$ide_detail$is_vscode || ide_info$ide_detail$is_positron) {
      cat("\n  NOTE: Automated AI detection is less complete in",
          ide_info$ide_name, "than in RStudio.\n")
      cat("  The manual checks above are especially important in this IDE.\n")
      cat("  Consider the two-computer approach for high-sensitivity work:\n")
      cat("  keep AI tools on a separate machine that never holds real data.\n")
    }
    cat("\n")
  }

  invisible(result)
}

#' Detect the current IDE and its AI risks
#'
#' Identifies whether the session is running in RStudio, Positron,
#' VS Code, or a plain terminal, and checks for IDE-specific AI
#' features that may transmit data.
#'
#' @param verbose Print results? Default \code{TRUE}.
#' @return Invisibly, a list with \code{ide_name}, \code{warnings},
#'   and \code{status}.
#' @export
audit_ide <- function(verbose = TRUE) {
  ide <- .detect_ide()
  warnings <- .ide_warnings(ide)

  status <- if (length(warnings) > 0) "AMBER" else "GREEN"

  result <- list(
    ide_name = ide$name,
    ide_detail = ide,
    warnings = warnings,
    status = status
  )

  if (verbose) {
    cat(sprintf("\n  IDE detected: %s\n", ide$name))
    if (length(warnings)) {
      for (w in warnings) cat("    ! ", w, "\n")
    } else {
      cat("    No IDE-level AI risks detected.\n")
    }
    cat("\n")
  }

  invisible(result)
}

#' Scan .Rprofile files for AI-related configuration
#'
#' Reads the user and project .Rprofile files and searches for
#' patterns that indicate AI auto-connections (e.g.,
#' \code{.chattr_chat}, \code{ellmer::chat_anthropic()}).
#'
#' @param verbose Print results? Default \code{TRUE}.
#' @return Invisibly, a list with \code{findings} and \code{status}.
#' @export
audit_rprofile <- function(verbose = TRUE) {

  files_to_check <- c(
    # User-level
    path.expand("~/.Rprofile"),
    # Project-level
    file.path(getwd(), ".Rprofile"),
    # Site-level
    Sys.getenv("R_PROFILE", unset = ""),
    file.path(R.home("etc"), "Rprofile.site")
  )
  files_to_check <- unique(files_to_check[nzchar(files_to_check)])

  findings <- list()

  for (fpath in files_to_check) {
    if (!file.exists(fpath)) next
    lines <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) character(0))
    for (i in seq_along(lines)) {
      line <- lines[i]
      # Skip comments
      stripped <- trimws(line)
      if (startsWith(stripped, "#")) next
      for (pat in .confider_rprofile_patterns) {
        if (grepl(pat, line, ignore.case = TRUE)) {
          findings <- c(findings, list(list(
            file = fpath,
            line_number = i,
            line_text = trimws(line),
            pattern = pat
          )))
          break  # one match per line is enough
        }
      }
    }
  }

  status <- if (length(findings) > 0) "RED" else "GREEN"

  result <- list(findings = findings, status = status)

  if (verbose) {
    if (length(findings)) {
      cat("\n  .Rprofile AI entries found:\n")
      for (f in findings) {
        cat(sprintf("    [%s:%d] %s\n", basename(f$file), f$line_number, f$line_text))
      }
      cat("\n  ACTION: Remove or comment out these lines before working\n")
      cat("  with confidential data. Run usethis::edit_r_profile() to edit.\n\n")
    } else {
      cat("\n  No AI-related entries found in .Rprofile files.\n\n")
    }
  }

  invisible(result)
}

#' Check for loaded AI-adjacent packages
#'
#' @param verbose Print results? Default \code{TRUE}.
#' @return Invisibly, a list with \code{loaded} and \code{status}.
#' @export
audit_packages <- function(verbose = TRUE) {
  loaded <- intersect(.confider_ai_packages, loadedNamespaces())
  # Also check attached packages
  attached <- intersect(
    paste0("package:", .confider_ai_packages),
    search()
  )
  attached_names <- sub("^package:", "", attached)
  all_active <- unique(c(loaded, attached_names))

  status <- if (length(all_active) > 0) "RED" else "GREEN"

  result <- list(loaded = all_active, status = status)

  if (verbose) {
    if (length(all_active)) {
      cat("\n  AI packages currently loaded:\n")
      for (p in all_active) cat("    ! ", p, "\n")
      cat("\n  These packages may transmit data to external servers.\n\n")
    } else {
      cat("\n  No AI packages loaded.\n\n")
    }
  }

  invisible(result)
}

#' Check for AI API keys in environment variables and .Renviron files
#'
#' Checks three locations: the live R environment (via Sys.getenv()),
#' the user-level .Renviron file (\code{~/.Renviron}), and the
#' project-level .Renviron file (\code{./.Renviron}). Keys in the
#' live environment can be cleared by confidential_mode_on(); keys
#' stored on disk persist and will be reloaded on next session.
#'
#' @param verbose Print results? Default \code{TRUE}.
#' @return Invisibly, a list with \code{found_live} (keys active in the
#'   environment), \code{found_on_disk} (keys stored in .Renviron files),
#'   and \code{status}.
#' @export
audit_env_keys <- function(verbose = TRUE) {

  # --- Check live environment ---
  found_live <- character(0)
  for (key in .confider_api_keys) {
    if (nzchar(Sys.getenv(key, unset = ""))) {
      found_live <- c(found_live, key)
    }
  }

  # --- Check .Renviron files on disk ---
  # These persist across sessions and will be reloaded.
  renviron_files <- unique(c(
    path.expand("~/.Renviron"),
    file.path(getwd(), ".Renviron"),
    Sys.getenv("R_ENVIRON_USER", unset = "")
  ))
  renviron_files <- renviron_files[nzchar(renviron_files)]

  found_on_disk <- list()
  for (fpath in renviron_files) {
    if (!file.exists(fpath)) next
    lines <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) character(0))
    for (i in seq_along(lines)) {
      line <- trimws(lines[i])
      if (!nzchar(line) || startsWith(line, "#")) next
      # .Renviron uses KEY=value format
      for (key in .confider_api_keys) {
        if (grepl(paste0("^", key, "\\s*="), line)) {
          found_on_disk <- c(found_on_disk, list(list(
            file = fpath,
            key = key,
            line_number = i
          )))
          break
        }
      }
    }
  }

  status <- if (length(found_live) > 0 || length(found_on_disk) > 0) "RED" else "GREEN"

  result <- list(
    found_live    = found_live,
    found_on_disk = found_on_disk,
    status        = status
  )

  if (verbose) {
    if (length(found_live)) {
      cat("\n  AI API keys in live environment:\n")
      for (k in found_live) cat("    ! ", k, "\n")
      cat("  These will be cleared by confidential_mode_on().\n")
    }
    if (length(found_on_disk)) {
      cat("\n  AI API keys in .Renviron files (persist across sessions):\n")
      for (f in found_on_disk) {
        cat(sprintf("    ! %s [%s:%d]\n", f$key, basename(f$file), f$line_number))
      }
      cat("  These files will reload keys on next R session.\n")
      cat("  Comment them out or move to a non-loaded location for stronger protection.\n")
    }
    if (length(found_live) == 0 && length(found_on_disk) == 0) {
      cat("\n  No AI API keys found in environment or .Renviron files.\n")
    }
    cat("\n")
  }

  invisible(result)
}

#' Print a one-line status summary
#'
#' @return Invisible \code{NULL}.
#' @export
confider_status <- function() {
  mode <- is_confidential_mode()
  hook <- isTRUE(getOption("confider.library_hook_active", FALSE))
  n_keys <- sum(vapply(.confider_api_keys, function(k) nzchar(Sys.getenv(k, "")), logical(1)))
  n_pkgs <- length(intersect(.confider_ai_packages, loadedNamespaces()))
  ide <- .detect_ide()

  cat(sprintf(
    "[confideR] mode=%s | hook=%s | keys=%d | ai_pkgs=%d | ide=%s\n",
    if (mode) "CONFIDENTIAL" else "open",
    if (hook) "active" else "off",
    n_keys, n_pkgs, ide$name
  ))
  invisible(NULL)
}

# ============================================================
# Internal: IDE detection
# ============================================================

.detect_ide <- function() {
  # --- Positron ---
  if (nzchar(Sys.getenv("POSITRON", ""))) {
    return(list(
      name = "Positron",
      is_positron = TRUE,
      is_rstudio = FALSE,
      is_vscode = FALSE,
      version = Sys.getenv("POSITRON_VERSION", "unknown")
    ))
  }

  # --- RStudio ---
  if (nzchar(Sys.getenv("RSTUDIO", ""))) {
    return(list(
      name = "RStudio",
      is_positron = FALSE,
      is_rstudio = TRUE,
      is_vscode = FALSE,
      version = Sys.getenv("RSTUDIO_VERSION", "unknown")
    ))
  }

  # --- VS Code ---
  if (nzchar(Sys.getenv("TERM_PROGRAM", "")) &&
      grepl("vscode", Sys.getenv("TERM_PROGRAM", ""), ignore.case = TRUE)) {
    return(list(
      name = "VS Code",
      is_positron = FALSE,
      is_rstudio = FALSE,
      is_vscode = TRUE,
      version = Sys.getenv("TERM_PROGRAM_VERSION", "unknown")
    ))
  }
  # Also check VSCODE_PID

  if (nzchar(Sys.getenv("VSCODE_PID", ""))) {
    return(list(
      name = "VS Code",
      is_positron = FALSE,
      is_rstudio = FALSE,
      is_vscode = TRUE,
      version = "unknown"
    ))
  }

  # --- Terminal / other ---
  list(
    name = "Terminal / Other",
    is_positron = FALSE,
    is_rstudio = FALSE,
    is_vscode = FALSE,
    version = NA_character_
  )
}

.ide_warnings <- function(ide) {
  warnings <- character(0)

  if (ide$is_positron) {
    # Positron: check for active AI configuration via environment
    # Positron sets specific env vars when Assistant features are active
    assistant_configured <- nzchar(Sys.getenv("ANTHROPIC_API_KEY", "")) ||
                            nzchar(Sys.getenv("OPENAI_API_KEY", ""))
    if (assistant_configured) {
      warnings <- c(warnings,
        "Positron Assistant has API keys available and may transmit console history, loaded data structures, and session state."
      )
    }

    # Copilot is enabled by default in Positron if you have a subscription,
    # but we can't easily detect it without a Positron-specific API.
    # Give a softer advisory.
    warnings <- c(warnings,
      "If you have a GitHub Copilot subscription, it is enabled by default in Positron. Check Settings > Positron Assistant to verify."
    )
  }

  if (ide$is_rstudio) {
    # Try to check Copilot status programmatically
    copilot_checked <- FALSE
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      tryCatch({
        if (rstudioapi::isAvailable()) {
          copilot_enabled <- rstudioapi::readRStudioPreference(
            "copilot_enabled", default = FALSE
          )
          copilot_checked <- TRUE
          if (isTRUE(copilot_enabled)) {
            warnings <- c(warnings,
              "GitHub Copilot is ENABLED. It transmits code context to GitHub's servers. Disable in Global Options > Copilot before working with confidential data."
            )
          }
          # If Copilot is disabled, no warning — that's GREEN.
        }
      }, error = function(e) NULL)
    }

    if (!copilot_checked) {
      # Couldn't verify programmatically (old RStudio, or rstudioapi unavailable)
      warnings <- c(warnings,
        "Could not verify Copilot status automatically. Check Global Options > Copilot if you are unsure."
      )
    }
  }

  if (ide$is_vscode) {
    # VS Code: we can't inspect extensions from R, but we can check
    # whether Claude Code or Copilot env markers are present
    copilot_signs <- nzchar(Sys.getenv("GITHUB_COPILOT_TOKEN", "")) ||
                     nzchar(Sys.getenv("GH_COPILOT_TOKEN", ""))
    if (copilot_signs) {
      warnings <- c(warnings,
        "GitHub Copilot appears to be active in VS Code. It transmits code context to GitHub's servers."
      )
    }
    # Claude Code sets specific env vars when running
    claude_code_signs <- nzchar(Sys.getenv("CLAUDE_CODE", ""))
    if (claude_code_signs) {
      warnings <- c(warnings,
        "Claude Code appears to be active. It can read workspace files and run commands, transmitting content to Anthropic's servers."
      )
    }
    if (!copilot_signs && !claude_code_signs) {
      # Can't fully verify from R — give a softer advisory
      warnings <- c(warnings,
        "Cannot fully verify VS Code extensions from R. Check the Extensions sidebar to confirm Copilot and Claude Code are disabled if working with confidential data."
      )
    }
  }

  warnings
}

.print_status_line <- function(label, value, status) {
  indicator <- switch(status,
    GREEN = "[OK]",
    AMBER = "[!!]",
    RED   = "[XX]",
    "[??]"
  )
  cat(sprintf("  %s %-25s %s\n", indicator, paste0(label, ":"), value))
}
