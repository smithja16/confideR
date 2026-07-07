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
  proc_info  <- audit_processes(verbose = FALSE)

  # Overall status
  statuses <- c(
    ide_info$status,
    pkg_info$status,
    key_info$status,
    if (!is.null(rprof_info)) rprof_info$status,
    proc_info$status
  )
  overall <- if ("RED" %in% statuses) "RED"
             else if ("AMBER" %in% statuses) "AMBER"
             else "GREEN"

  result <- list(
    ide       = ide_info,
    packages  = pkg_info,
    env_keys  = key_info,
    rprofile  = rprof_info,
    processes = proc_info,
    confidential_mode = is_confidential_mode(),
    overall_status    = overall
  )

  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("   confideR: Session Audit Report\n")
    cat("========================================\n\n")

    # Status key — explains the [OK]/[!!]/[XX] indicators used below
    cat("  Status key:  [OK] no risk   [!!] check advised   [XX] action needed\n\n")

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
      if (isTRUE(ide_info$ide_detail$is_workbench)) {
        cat("    [i] Posit Workbench detected. confideR can inspect Copilot settings\n")
        cat("        via rstudioapi, but server-level AI features are not visible to R.\n")
      } else {
        cat("    [i] RStudio detected. confideR can inspect Copilot settings\n")
        cat("        directly via rstudioapi.\n")
      }
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
    live_keys   <- key_info$found_live
    disk_keys   <- unique(vapply(key_info$found_on_disk, function(f) f$key, character(1)))
    dotenv_keys <- unique(vapply(key_info$found_dotenv,  function(f) f$key, character(1)))
    all_disk    <- unique(c(disk_keys, dotenv_keys))
    if (length(live_keys)) {
      .print_status_line("AI API keys", paste(unique(live_keys), collapse = ", "), key_info$status)
      cat("    [i] Key(s) LIVE in environment now.\n")
      if (length(setdiff(disk_keys, live_keys))) {
        cat("        Also in .Renviron:", paste(setdiff(disk_keys, live_keys), collapse = ", "), "\n")
      }
      if (length(setdiff(dotenv_keys, live_keys))) {
        cat("        Also in .env:", paste(setdiff(dotenv_keys, live_keys), collapse = ", "), "\n")
      }
    } else if (length(all_disk)) {
      .print_status_line("AI API keys", paste(all_disk, collapse = ", "), key_info$status)
      if (length(disk_keys)) {
        if (isTRUE(key_info$confidential_mode)) {
          cat("    [i] Cleared from this session; still in .Renviron (reloads next session).\n")
        } else {
          cat("    [i] In .Renviron only; not yet loaded into this session.\n")
        }
      }
      if (length(dotenv_keys)) {
        cat("    [i] In .env; R does not load .env automatically — exposed only if a\n")
        cat("        tool loads it (dotenv, python-dotenv, shell export). See audit_env_keys().\n")
      }
    } else {
      .print_status_line("AI API keys", "(none)", key_info$status)
    }

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

    # AI options (residual from packages used earlier in session)
    if (length(pkg_info$ai_options)) {
      cat("    ! AI-related R options set (package may have been active earlier):",
          paste(pkg_info$ai_options, collapse = ", "), "\n")
    }

    # Processes
    .print_status_line("AI processes",
      if (length(proc_info$found)) paste(proc_info$found, collapse = ", ")
      else "(none)",
      proc_info$status
    )
    if (length(proc_info$found)) {
      cat("    [i] Process detected on this machine but connection to this\n")
      cat("        R session is not confirmed. Verify manually.\n")
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
      if (isTRUE(ide_info$ide_detail$is_workbench)) {
        cat("    [ ] Contact your server administrator to confirm no server-level\n")
        cat("        AI features (Workbench Assistant) are enabled for this user\n")
      }
      cat("    [ ] Global Options > Copilot: confirm Copilot is disabled\n")
      cat("        (confideR checks this via rstudioapi, but verify visually\n")
      cat("        if the automated check returned AMBER or could not verify)\n")
      cat("    [ ] Addins menu: check no AI addins (gptstudio, gander, chattr)\n")
      cat("        are active or have been invoked in this session\n")
    } else if (ide_info$ide_detail$is_positron) {
      cat("    [ ] Settings > Positron Assistant: confirm the Assistant is\n")
      cat("        disabled or set to a local-only model\n")
      cat("        (confideR checks settings.json, but verify visually)\n")
      cat("    [ ] Settings > Extensions > GitHub Copilot: confirm Copilot\n")
      cat("        is disabled (confideR checks settings.json, but verify visually)\n")
      cat("    [ ] Check that your R console history does not show data output\n")
      cat("        from head(), print(), or summary() calls — Positron Assistant\n")
      cat("        reads console history as context\n")
    } else if (ide_info$ide_detail$is_vscode) {
      cat("    [ ] Extensions sidebar: confirm Copilot and other AI extensions\n")
      cat("        are disabled (confideR scans ~/.vscode/extensions/ but cannot\n")
      cat("        read per-workspace enabled/disabled state)\n")
      cat("    [ ] Check VS Code settings.json for AI extension configuration\n")
      cat("        (Ctrl+Shift+P > 'Open User Settings JSON')\n")
      cat("    [ ] Be aware that some VS Code extensions scan terminal output\n")
      cat("        to attach to R sessions without loading any R package —\n")
      cat("        these cannot be detected from within R\n")
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

  # Scan R options for AI package configuration set earlier in session
  # (catches packages that were loaded and configured but since unloaded).
  ai_opts <- .scan_ai_options()

  status <- if (length(all_active) > 0 || length(ai_opts) > 0) "RED" else "GREEN"

  result <- list(loaded = all_active, ai_options = ai_opts, status = status)

  if (verbose) {
    if (length(all_active)) {
      cat("\n  AI packages currently loaded:\n")
      for (p in all_active) cat("    ! ", p, "\n")
      cat("\n  These packages may transmit data to external servers.\n\n")
    } else {
      cat("\n  No AI packages loaded.\n\n")
    }
    if (length(ai_opts)) {
      cat("  AI-related R options detected (may indicate prior AI package use):\n")
      for (o in ai_opts) cat("    ! ", o, "\n")
      cat("\n")
    }
  }

  invisible(result)
}

#' Check for AI API keys in environment variables, .Renviron, and .env files
#'
#' Checks the live R environment (via Sys.getenv()), the user- and
#' project-level .Renviron files (\code{~/.Renviron}, \code{./.Renviron}),
#' and .env files (\code{./.env}, \code{~/.env}). Keys in the live
#' environment can be cleared by confidential_mode_on(). Keys in .Renviron
#' persist and reload automatically next session. Keys in .env are NOT read
#' by R automatically — they only reach the session if a tool loads them
#' (e.g. \code{dotenv::load_dot_env()}, or a Python step via reticulate).
#' confideR only reads these files; it never modifies them.
#'
#' @param verbose Print results? Default \code{TRUE}.
#' @return Invisibly, a list with \code{found_live} (keys active in the
#'   environment), \code{found_on_disk} (keys in .Renviron files),
#'   \code{found_dotenv} (keys in .env files), and \code{status}.
#' @export
audit_env_keys <- function(verbose = TRUE) {

  conf_mode <- is_confidential_mode()

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

  # --- Check .env files on disk (dotenv format) ---
  # Unlike .Renviron, R does NOT read these automatically; a key here is only
  # a live exposure once a tool loads it. confideR only reads them, never edits.
  found_dotenv <- .scan_dotenv_keys()

  # Keys that are on disk but NOT currently live. When confidential mode
  # is active, live keys have been cleared, so an on-disk key that is not
  # live has been successfully neutralised for this session (though it will
  # reload next session unless removed from .Renviron).
  on_disk_keys <- unique(vapply(found_on_disk, function(f) f$key, character(1)))
  on_disk_not_live <- setdiff(on_disk_keys, found_live)

  # --- Status logic ---
  # RED   : a key is live in the environment right now (active exposure)
  # AMBER : no live keys, but keys exist in .Renviron or .env AND confidential
  #         mode is OFF — a latent risk the user should be aware of (.Renviron
  #         will load next session; .env could be loaded by a tool this session)
  # GREEN : no keys anywhere, OR the only on-disk keys exist and confidential
  #         mode is ON (already cleared/not loaded this session; the on-disk
  #         copy is a next-session concern, not a current exposure)
  status <- if (length(found_live) > 0) {
    "RED"
  } else if ((length(found_on_disk) > 0 || length(found_dotenv) > 0) && !conf_mode) {
    "AMBER"
  } else {
    "GREEN"
  }

  result <- list(
    found_live       = found_live,
    found_on_disk    = found_on_disk,
    found_dotenv     = found_dotenv,
    on_disk_not_live = on_disk_not_live,
    confidential_mode = conf_mode,
    status           = status
  )

  if (verbose) {
    if (length(found_live)) {
      cat("\n  AI API keys LIVE in environment (active exposure):\n")
      for (k in found_live) cat("    ! ", k, "\n")
      if (conf_mode) {
        cat("  NOTE: confidential mode is active but these keys are still live.\n")
        cat("  They may have been set after confidential_mode_on() was called.\n")
      } else {
        cat("  These will be cleared by confidential_mode_on().\n")
      }
    }
    if (length(found_on_disk)) {
      cat("\n  AI API keys in .Renviron files (persist across sessions):\n")
      for (f in found_on_disk) {
        live_flag <- if (f$key %in% found_live) " [currently LIVE]"
                     else if (conf_mode) " [cleared from this session, still on disk]"
                     else " [will load next session]"
        cat(sprintf("    ! %s [%s:%d]%s\n", f$key, basename(f$file), f$line_number, live_flag))
      }
      if (conf_mode && !length(found_live)) {
        cat("  These keys have been cleared from the current session by\n")
        cat("  confidential mode, but remain in .Renviron and will reload\n")
        cat("  on next R session. Comment them out for permanent removal.\n")
      } else {
        cat("  Comment them out or move to a non-loaded location for stronger protection.\n")
      }
    }
    if (length(found_dotenv)) {
      cat("\n  AI API keys in .env files (dotenv format):\n")
      for (f in found_dotenv) {
        live_flag <- if (f$key %in% found_live) " [currently LIVE in session]"
                     else " [not loaded into this session]"
        cat(sprintf("    ! %s [%s:%d]%s\n", f$key, basename(f$file), f$line_number, live_flag))
      }
      cat("  Unlike .Renviron, R does NOT read .env automatically, so these keys\n")
      cat("  are only exposed if something loads them. Common ways that happens\n")
      cat("  (sometimes without an obvious step):\n")
      cat("    - dotenv::load_dot_env() or readRenviron('.env') in a script or .Rprofile\n")
      cat("    - a Python step via reticulate using python-dotenv\n")
      cat("    - the shell, a Makefile, or a Docker step exporting them into R's parent process\n")
      cat("  confideR only reads .env files; it never edits or clears them. To remove\n")
      cat("  a key, delete or comment its line, or avoid loading the file this session.\n")
    }
    if (length(found_live) == 0 && length(found_on_disk) == 0 && length(found_dotenv) == 0) {
      cat("\n  No AI API keys found in environment, .Renviron, or .env files.\n")
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

  # --- RStudio / Posit Workbench ---
  if (nzchar(Sys.getenv("RSTUDIO", ""))) {
    is_workbench <- grepl("server", Sys.getenv("RSTUDIO_PROGRAM_MODE", ""),
                          ignore.case = TRUE)
    return(list(
      name = if (is_workbench) "Posit Workbench" else "RStudio",
      is_positron = FALSE,
      is_rstudio = TRUE,
      is_workbench = is_workbench,
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
    settings <- .read_positron_settings()

    if (isTRUE(settings$copilot)) {
      warnings <- c(warnings,
        "GitHub Copilot is ENABLED in Positron settings. It transmits code context to GitHub's servers. Disable in Settings > Extensions > GitHub Copilot."
      )
    } else if (is.na(settings$copilot)) {
      warnings <- c(warnings,
        "Could not read Copilot status from Positron settings.json. If you have a GitHub Copilot subscription, verify it is disabled in Settings > Extensions > GitHub Copilot."
      )
    }
    # settings$copilot == FALSE: confirmed disabled, no warning.

    if (isTRUE(settings$assistant)) {
      warnings <- c(warnings,
        "Positron Assistant is ENABLED. It may transmit console history and session context to external servers. Disable in Settings > Positron Assistant."
      )
    }
    # assistant == FALSE or NA with no keys: no advisory — avoids alert fatigue.

    # API keys as a secondary signal only when settings could not be read.
    if (is.na(settings$assistant)) {
      assistant_keys <- nzchar(Sys.getenv("ANTHROPIC_API_KEY", "")) ||
                        nzchar(Sys.getenv("OPENAI_API_KEY", ""))
      if (assistant_keys) {
        warnings <- c(warnings,
          "AI API keys are present in the environment. If Positron Assistant is configured, it may use them to transmit console history as context."
        )
      }
    }
  }

  if (ide$is_rstudio) {
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
        }
      }, error = function(e) NULL)
    }

    if (!copilot_checked) {
      warnings <- c(warnings,
        "Could not verify Copilot status automatically. Check Global Options > Copilot if you are unsure."
      )
    }

    if (isTRUE(ide$is_workbench)) {
      warnings <- c(warnings,
        "Posit Workbench detected. Server-level AI features (e.g. Workbench Assistant) are not visible to R. Check with your server administrator."
      )
    }
  }

  if (ide$is_vscode) {
    # Extension directory scan — more reliable than env var proxies.
    found_exts <- .scan_vscode_extensions()
    if (length(found_exts)) {
      warnings <- c(warnings, paste0(
        "AI-related VS Code extensions detected in ~/.vscode/extensions/: ",
        paste(found_exts, collapse = ", "),
        ". Verify these are disabled for this workspace."
      ))
    }

    # Copilot token env vars (present in some configurations).
    copilot_signs <- nzchar(Sys.getenv("GITHUB_COPILOT_TOKEN", "")) ||
                     nzchar(Sys.getenv("GH_COPILOT_TOKEN", ""))
    if (copilot_signs) {
      warnings <- c(warnings,
        "GitHub Copilot token found in environment. Copilot is active and transmits code context to GitHub's servers."
      )
    }

    # Claude Code: check workspace .claude/ directory (more reliable than
    # CLAUDE_CODE env var, which Claude Code does not actually set).
    claude_signs <- file.exists(file.path(getwd(), ".claude")) ||
                    file.exists(file.path(path.expand("~"), ".claude"))
    if (claude_signs) {
      warnings <- c(warnings,
        "Claude Code data directory (.claude/) found. Claude Code has been used in this workspace and can read files and transmit content to Anthropic's servers."
      )
    }

    if (!length(found_exts) && !copilot_signs && !claude_signs) {
      warnings <- c(warnings,
        "Cannot fully verify VS Code AI extensions from R. Check the Extensions sidebar to confirm Copilot and other AI tools are disabled."
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

# ============================================================
# audit_processes() — system process table scan
# ============================================================

#' Scan the system process table for active AI agent processes
#'
#' Queries the OS process list for process names associated with known
#' AI coding tools (GitHub Copilot language server, Codeium, Tabnine,
#' etc.) and local model servers (Ollama, LM Studio, llama.cpp, and
#' others). Returns AMBER if any are found — a detected process indicates
#' the tool is running on this machine but does not confirm it is
#' connected to the current R session.
#'
#' Local model servers are included because they accept connections over
#' localhost HTTP with no R package loaded and no API key set, making them
#' otherwise invisible to confideR's other checks.
#'
#' If the \code{ps} package is installed, it is used for structured,
#' cross-platform process inspection including full command lines. If not,
#' the function falls back to \code{system2()} with \code{tasklist}
#' (Windows) or \code{ps aux} (macOS/Linux). Installing \code{ps} is
#' recommended for more reliable detection. If the process scan fails
#' (e.g. a restricted environment), the function returns GREEN with a
#' note rather than erroring.
#'
#' @param verbose Print results? Default \code{TRUE}.
#' @return Invisibly, a list with \code{found} (matched process name
#'   fragments) and \code{status}.
#' @export
audit_processes <- function(verbose = TRUE) {
  procs <- .get_process_list()

  found <- character(0)
  if (length(procs)) {
    for (pat in .confider_ai_processes) {
      if (any(grepl(pat, procs, ignore.case = TRUE, perl = TRUE))) {
        found <- c(found, pat)
      }
    }
  }

  status <- if (length(found) > 0) "AMBER" else "GREEN"
  result <- list(found = found, status = status)

  if (verbose) {
    if (!length(procs)) {
      cat("\n  Process scan unavailable (system call returned no output).\n\n")
    } else if (length(found)) {
      cat("\n  AI-related processes detected on this machine:\n")
      for (p in found) cat("    ! ", p, "\n")
      cat("\n  A detected process does not confirm it is connected to this\n")
      cat("  R session. Verify extension status manually.\n\n")
    } else {
      cat("\n  No AI-related processes detected.\n\n")
    }
  }

  invisible(result)
}

# Returns a character vector of running processes, one element per process,
# combining the process name and (where available) the full command line.
#
# Uses the 'ps' package when installed: ps::ps() returns a structured,
# cross-platform data frame, and ps::ps_cmdline() exposes the full command
# line — important because tools like Ollama appear as "ollama serve" and
# Node-based agents appear as a generic "node" with the tool in the args.
#
# Falls back to system2() with tasklist (Windows) or ps aux (Unix) when the
# 'ps' package is not installed. The fallback is less reliable: output
# formats differ by platform and long command names may be truncated.
.get_process_list <- function() {
  # Preferred path: the 'ps' package (structured, cross-platform).
  if (requireNamespace("ps", quietly = TRUE)) {
    out <- tryCatch({
      pl <- ps::ps()
      if (!nrow(pl)) return(character(0))
      vapply(seq_len(nrow(pl)), function(i) {
        name <- pl$name[i]
        # Append the full command line where accessible; some processes
        # (e.g. those owned by other users) will deny access — ignore those.
        cmd <- tryCatch(
          paste(ps::ps_cmdline(pl$ps_handle[[i]]), collapse = " "),
          error = function(e) ""
        )
        trimws(paste(name, cmd))
      }, character(1))
    }, error = function(e) NULL)

    if (!is.null(out)) return(out)
    # If ps failed at runtime, fall through to the system2 fallback.
  }

  .get_process_list_fallback()
}

# Fallback process list using base R system2(). Less reliable than 'ps':
# - tasklist and 'ps aux' produce different output formats
# - 'ps aux' may truncate long command names
# - 'ps' may be absent or localised on restricted systems
.get_process_list_fallback <- function() {
  tryCatch(
    if (.Platform$OS.type == "windows") {
      system2("tasklist", stdout = TRUE, stderr = FALSE)
    } else {
      # -ww prevents truncation of long command lines where supported
      system2("ps", args = c("aux", "-ww"), stdout = TRUE, stderr = FALSE)
    },
    error   = function(e) character(0),
    warning = function(w) character(0)
  )
}

# ============================================================
# .env (dotenv) file scan
# ============================================================

# Scans .env files for AI API keys. Unlike .Renviron, .env files are NOT
# read by R automatically — a key here only reaches the session if a tool
# loads it (dotenv::load_dot_env(), python-dotenv via reticulate, a shell
# export, etc.). confideR only READS these files; it never edits or clears
# them. Returns a list of list(file, key, line_number), matching the shape
# of found_on_disk so downstream reporting can treat them uniformly.
#
# Tolerant of the common dotenv line forms:
#   KEY=value
#   export KEY=value
#   KEY="value" / KEY='value'
.scan_dotenv_keys <- function() {
  dotenv_files <- unique(c(
    file.path(getwd(), ".env"),
    path.expand("~/.env")
  ))
  dotenv_files <- dotenv_files[nzchar(dotenv_files)]

  found <- list()
  for (fpath in dotenv_files) {
    if (!file.exists(fpath)) next
    lines <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) character(0))
    for (i in seq_along(lines)) {
      line <- trimws(lines[i])
      if (!nzchar(line) || startsWith(line, "#")) next
      # Strip an optional leading "export " so the KEY= match works.
      line_key <- sub("^export\\s+", "", line)
      for (key in .confider_api_keys) {
        if (grepl(paste0("^", key, "\\s*="), line_key)) {
          found <- c(found, list(list(
            file = fpath,
            key = key,
            line_number = i
          )))
          break
        }
      }
    }
  }
  found
}

# ============================================================
# VS Code extension directory scan
# ============================================================

.scan_vscode_extensions <- function() {
  ext_dir <- if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("USERPROFILE"), ".vscode", "extensions")
  } else {
    file.path(path.expand("~"), ".vscode", "extensions")
  }

  if (!dir.exists(ext_dir)) return(character(0))

  ext_names <- list.dirs(ext_dir, full.names = FALSE, recursive = FALSE)

  found <- character(0)
  for (pat in .confider_vscode_ext_patterns) {
    matches <- ext_names[grepl(pat, ext_names, ignore.case = TRUE, perl = TRUE)]
    found <- c(found, matches)
  }
  unique(found)
}

# ============================================================
# Positron settings.json helpers
# ============================================================

.positron_settings_path <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Windows") {
    file.path(Sys.getenv("APPDATA"), "Positron", "User", "settings.json")
  } else if (sysname == "Darwin") {
    file.path(path.expand("~"), "Library", "Application Support",
              "Positron", "User", "settings.json")
  } else {
    file.path(path.expand("~"), ".config", "Positron", "User", "settings.json")
  }
}

# Returns list(copilot = TRUE/FALSE/NA, assistant = TRUE/FALSE/NA).
# NA means the key was absent from settings.json or the file could not be read.
.read_positron_settings <- function() {
  path <- .positron_settings_path()
  if (!file.exists(path)) return(list(copilot = NA, assistant = NA))

  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  if (!length(lines)) return(list(copilot = NA, assistant = NA))

  text <- paste(lines, collapse = "\n")

  copilot_on  <- grepl('"github\\.copilot\\.enable"\\s*:\\s*true',  text, perl = TRUE)
  copilot_off <- grepl('"github\\.copilot\\.enable"\\s*:\\s*false', text, perl = TRUE)
  copilot <- if (copilot_on) TRUE else if (copilot_off) FALSE else NA

  asst_on  <- grepl('"positron\\.assistant\\.enable"\\s*:\\s*true',  text, perl = TRUE,
                    ignore.case = TRUE)
  asst_off <- grepl('"positron\\.assistant\\.enable"\\s*:\\s*false', text, perl = TRUE,
                    ignore.case = TRUE)
  assistant <- if (asst_on) TRUE else if (asst_off) FALSE else NA

  list(copilot = copilot, assistant = assistant)
}

# ============================================================
# R options scan
# ============================================================

.scan_ai_options <- function() {
  all_opts <- names(options())

  # Exclude confideR's own options (e.g. confider.backup.openai_api_key),
  # which would otherwise match AI patterns and cause the package to
  # detect itself as an AI connection.
  all_opts <- all_opts[!grepl("^confider\\.", all_opts, ignore.case = TRUE)]

  found <- character(0)
  for (pat in .confider_ai_option_patterns) {
    matches <- all_opts[grepl(pat, all_opts, ignore.case = TRUE, perl = TRUE)]
    found <- c(found, matches)
  }
  unique(found)
}
