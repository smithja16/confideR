############################################################
# confideR – Confidential mode
#
# Provides the core session protection mechanism. When
# confidential mode is active:
#   - AI API keys are cleared from the environment
#   - Known AI packages are unloaded
#   - library() is hooked to block AI packages
#   - A session flag gates data loading and AI calls
#
# Call confidential_mode_on() BEFORE loading any data.
############################################################

#' Activate confidential mode
#'
#' Call at the start of a session before loading any confidential data.
#' This function:
#' \itemize{
#'   \item Sets the \code{confider.confidential_mode} flag
#'   \item Backs up and clears all registered AI API keys
#'   \item Unloads any currently loaded AI-adjacent packages
#'   \item Installs a hook on \code{library()} to block AI packages
#'   \item Runs a quick audit and prints a status summary
#' }
#'
#' @param verbose Logical. Print status messages? Default \code{TRUE}.
#' @return Invisible \code{TRUE}.
#' @export
#' @examples
#' confidential_mode_on()
#' is_confidential_mode()
#' confidential_mode_off()
confidential_mode_on <- function(verbose = TRUE) {

  # --- Idempotency check ---
  # If already in confidential mode, don't re-back up (which would
  # overwrite the real backup with empty strings and lose the keys).
  if (is_confidential_mode()) {
    if (verbose) {
      cat("[confideR] Confidential mode is already active. No action taken.\n")
      cat("  To refresh after changes, call confidential_mode_off() then confidential_mode_on().\n")
    }
    return(invisible(TRUE))
  }

  options(confider.confidential_mode = TRUE)

  # --- Identify keys sourced from .Renviron (durable) vs. interactively-set ---
  # .Renviron-sourced keys will reappear on next session even without restore.
  # Interactively-set keys exist only in the live environment; if not restored,
  # the user will need to re-enter them.
  renviron_keys <- .read_renviron_keys()

  # --- Back up and clear API keys ---
  backed_up           <- character(0)
  interactive_only    <- character(0)  # keys that would be lost if R crashes
  for (key in .confider_api_keys) {
    val <- Sys.getenv(key, unset = "")
    if (nzchar(val)) {
      opt_name <- paste0("confider.backup.", tolower(gsub("[^A-Za-z0-9]", "_", key)))
      # Extra safety: if a backup already exists (from a previous session that
      # didn't call _off), keep the first one — don't overwrite with a value
      # that might itself have come from a stale restore.
      existing <- getOption(opt_name, NULL)
      if (is.null(existing) || !nzchar(existing)) {
        do.call(options, setNames(list(val), opt_name))
      }
      Sys.unsetenv(key)
      backed_up <- c(backed_up, key)
      if (!key %in% renviron_keys) {
        interactive_only <- c(interactive_only, key)
      }
    }
  }

  # --- Unload AI packages ---
  loaded_ai <- intersect(.confider_ai_packages, loadedNamespaces())
  unloaded <- character(0)
  for (pkg in loaded_ai) {
    tryCatch({
      unloadNamespace(pkg)
      unloaded <- c(unloaded, pkg)
    }, error = function(e) {
      if (verbose) message(
        "[confideR] Could not unload: ", pkg, " (", e$message, ")"
      )
    })
  }

  # --- Install library hook ---
  .install_library_hook()

  # --- Report ---
  if (verbose) {
    cat("\n")
    cat("=== confideR: Confidential Mode ACTIVE ===\n\n")
    if (length(backed_up)) {
      cat("  API keys backed up & cleared: ", paste(backed_up, collapse = ", "), "\n")
      cat("    (Restored automatically by confidential_mode_off())\n")
      if (length(interactive_only)) {
        cat("\n  NOTE: the following key(s) exist only in this session\n")
        cat("  (not in .Renviron) and will need to be re-entered manually\n")
        cat("  if R is closed before confidential_mode_off() is called:\n")
        for (k in interactive_only) cat("    - ", k, "\n")
        cat("  If this matters to you, add them to ~/.Renviron now, before proceeding.\n")
      }
    } else {
      cat("  API keys:              (none found in environment)\n")
    }
    if (length(unloaded)) {
      cat("  Packages unloaded:     ", paste(unloaded, collapse = ", "), "\n")
    } else {
      cat("  Packages unloaded:      (none were loaded)\n")
    }
    cat("  library() hook:         active (AI packages blocked)\n")
    cat("\n")

    # Quick audit of IDE-level AI features
    ide <- .detect_ide()
    warnings <- .ide_warnings(ide)
    if (length(warnings)) {
      cat("  IDE warnings:\n")
      for (w in warnings) cat("    ! ", w, "\n")
      cat("\n")
    }

    cat("  Automated protections are now active. However, confideR\n")
    cat("  cannot detect all AI features (e.g. IDE-level extensions,\n")
    cat("  unlisted packages, or shared workstation configurations).\n")
    cat("  Please also verify manually that AI tools are disabled.\n")
    cat("\n  To exit: confidential_mode_off()\n\n")
  }

  invisible(TRUE)
}

#' Deactivate confidential mode
#'
#' Reverts all protections installed by \code{confidential_mode_on()}:
#' restores backed-up API keys, removes the library hook, and clears
#' the session flag.
#'
#' @param verbose Logical. Print status messages? Default \code{TRUE}.
#' @return Invisible \code{TRUE}.
#' @export
confidential_mode_off <- function(verbose = TRUE) {

  options(confider.confidential_mode = FALSE)

  # --- Restore API keys ---
  restored <- character(0)
  for (key in .confider_api_keys) {
    opt_name <- paste0("confider.backup.", tolower(gsub("[^A-Za-z0-9]", "_", key)))
    val <- getOption(opt_name, NULL)
    if (!is.null(val) && nzchar(val)) {
      do.call(Sys.setenv, setNames(list(val), key))
      do.call(options, setNames(list(NULL), opt_name))
      restored <- c(restored, key)
    }
  }

  # --- Remove library hook ---
  .remove_library_hook()

  if (verbose) {
    cat("\n=== confideR: Confidential Mode OFF ===\n")
    if (length(restored)) {
      cat("  API keys restored: ", paste(restored, collapse = ", "), "\n")
    }
    cat("  library() hook removed.\n")
    cat("  WARNING: AI tools may now transmit data. Do not load\n")
    cat("  confidential data in this session.\n\n")
  }

  invisible(TRUE)
}

#' Check whether confidential mode is active
#'
#' @return Logical scalar.
#' @export
is_confidential_mode <- function() {
  isTRUE(getOption("confider.confidential_mode", FALSE))
}

#' Require confidential mode (internal guard)
#'
#' Place at the top of functions that touch raw data. Raises an error
#' if confidential mode is not active.
#'
#' @param reason Optional message appended to the error.
#' @return Invisible \code{TRUE}.
#' @keywords internal
require_confidential_mode <- function(reason = NULL) {
  if (!is_confidential_mode()) {
    msg <- paste0("[confideR] Confidential mode is not active.",
                  " Call confidential_mode_on() first.")
    if (!is.null(reason)) msg <- paste(msg, reason)
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Block AI calls in confidential mode (internal guard)
#'
#' Place at the top of any wrapper that contacts an AI API.
#'
#' @param reason Optional message.
#' @return Invisible \code{TRUE}.
#' @keywords internal
prevent_ai_in_confidential_mode <- function(reason = NULL) {
  if (is_confidential_mode()) {
    msg <- paste0(
      "[confideR] AI calls are blocked in confidential mode. ",
      "Use confidential_mode_off() first, or work in a separate session."
    )
    if (!is.null(reason)) msg <- paste(msg, reason)
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

# ============================================================
# Library hook (blocks AI packages via packageEvent)
# ============================================================

.install_library_hook <- function() {
  # Use a factory to avoid the closure-in-loop capture bug:
  # without this, `pkg_name` in the hook would always be the
  # last value of the loop variable.
  make_hook <- function(pkg_name) {
    function(...) {
      if (is_confidential_mode()) {
        stop(
          sprintf(
            "[confideR] Package '%s' is blocked in confidential mode. ",
            pkg_name
          ),
          "It is on confideR's AI package blocklist. ",
          "Use confidential_mode_off() first if you need this package.",
          call. = FALSE
        )
      }
    }
  }
  for (pkg in .confider_ai_packages) {
    setHook(packageEvent(pkg, "onLoad"), make_hook(pkg))
  }
  options(confider.library_hook_active = TRUE)
}

.remove_library_hook <- function() {
  for (pkg in .confider_ai_packages) {
    setHook(packageEvent(pkg, "onLoad"), NULL, action = "replace")
  }
  options(confider.library_hook_active = FALSE)
}

# ============================================================
# Helpers: detecting which keys are durable (.Renviron-backed)
# ============================================================

# Returns the names of API keys that are defined in .Renviron files on disk
# (either user-level or project-level). These keys will automatically
# reappear on the next R session and do NOT need to be manually re-entered
# if confidential_mode_off() is skipped.
.read_renviron_keys <- function() {
  renviron_files <- unique(c(
    path.expand("~/.Renviron"),
    file.path(getwd(), ".Renviron"),
    Sys.getenv("R_ENVIRON_USER", unset = "")
  ))
  renviron_files <- renviron_files[nzchar(renviron_files)]

  found <- character(0)
  for (fpath in renviron_files) {
    if (!file.exists(fpath)) next
    lines <- tryCatch(readLines(fpath, warn = FALSE), error = function(e) character(0))
    for (line in lines) {
      line <- trimws(line)
      if (!nzchar(line) || startsWith(line, "#")) next
      for (key in .confider_api_keys) {
        if (grepl(paste0("^", key, "\\s*="), line)) {
          found <- c(found, key)
        }
      }
    }
  }
  unique(found)
}

# ============================================================
# Recovery helpers
# ============================================================

#' Restore API keys without turning off confidential mode
#'
#' Emergency/manual recovery: if you need to re-enable AI tools
#' temporarily but want to keep the library hook and session flag
#' active, this function restores the backed-up keys without calling
#' confidential_mode_off(). Use sparingly — the mental model is that
#' keys and protection travel together, and separating them is risky.
#'
#' @param verbose Print status? Default \code{TRUE}.
#' @return Invisibly, the names of restored keys.
#' @export
restore_api_keys <- function(verbose = TRUE) {
  restored <- character(0)
  for (key in .confider_api_keys) {
    opt_name <- paste0("confider.backup.", tolower(gsub("[^A-Za-z0-9]", "_", key)))
    val <- getOption(opt_name, NULL)
    if (!is.null(val) && nzchar(val)) {
      do.call(Sys.setenv, setNames(list(val), key))
      restored <- c(restored, key)
    }
  }

  if (verbose) {
    if (length(restored)) {
      cat("[confideR] Restored ", length(restored), " API key(s): ",
          paste(restored, collapse = ", "), "\n", sep = "")
      if (is_confidential_mode()) {
        cat("  Confidential mode is still ACTIVE. AI packages remain blocked.\n")
        cat("  Call confidential_mode_off() to fully exit protection.\n")
      }
    } else {
      cat("[confideR] No backed-up keys to restore.\n")
      cat("  If keys were set interactively and R was restarted, they are gone\n")
      cat("  from memory. Re-enter them with Sys.setenv() or from .Renviron.\n")
    }
  }
  invisible(restored)
}

#' Show the status of backed-up API keys
#'
#' Useful when a user is unsure whether their keys are safely backed
#' up or already gone. Lists which keys are currently in the live
#' environment, which are backed up in options(), and which are stored
#' in .Renviron files on disk.
#'
#' @return Invisibly, a list with components \code{live}, \code{backed_up},
#'   and \code{on_disk}.
#' @export
api_key_status <- function() {
  live <- character(0); backed_up <- character(0)
  for (key in .confider_api_keys) {
    if (nzchar(Sys.getenv(key, unset = ""))) live <- c(live, key)
    opt_name <- paste0("confider.backup.", tolower(gsub("[^A-Za-z0-9]", "_", key)))
    if (!is.null(getOption(opt_name, NULL)) && nzchar(getOption(opt_name, ""))) {
      backed_up <- c(backed_up, key)
    }
  }
  on_disk <- .read_renviron_keys()

  cat("\n=== confideR: API key status ===\n\n")
  cat("  Live in environment (", length(live), "): ",
      if (length(live)) paste(live, collapse = ", ") else "(none)", "\n", sep = "")
  cat("  Backed up by confideR (", length(backed_up), "): ",
      if (length(backed_up)) paste(backed_up, collapse = ", ") else "(none)", "\n", sep = "")
  cat("  In .Renviron on disk (", length(on_disk), "): ",
      if (length(on_disk)) paste(on_disk, collapse = ", ") else "(none)", "\n", sep = "")

  # Diagnose the situation
  cat("\n")
  if (length(backed_up) && !length(live)) {
    cat("  Interpretation: keys are cleared and safely backed up.\n")
    cat("  confidential_mode_off() or restore_api_keys() will restore them.\n")
  } else if (length(backed_up) && length(live)) {
    cat("  Interpretation: some keys have been restored to the environment,\n")
    cat("  but backups remain. This is the normal state after restore_api_keys().\n")
  } else if (!length(backed_up) && !length(live) && length(on_disk)) {
    cat("  Interpretation: no live keys and no backups, but .Renviron has\n")
    cat("  keys that will reload next session.\n")
  } else if (!length(backed_up) && !length(live) && !length(on_disk)) {
    cat("  Interpretation: no AI keys anywhere. If you need them, set with\n")
    cat("  Sys.setenv() or add to .Renviron.\n")
  } else if (length(live) && !length(backed_up) && !is_confidential_mode()) {
    cat("  Interpretation: keys are live, confidential mode is off. Normal.\n")
  }
  cat("\n")

  invisible(list(live = live, backed_up = backed_up, on_disk = on_disk))
}
