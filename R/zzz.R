############################################################
# safecatch – Package load hooks
############################################################

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "safecatch ", utils::packageVersion("safecatch"), " loaded.\n",
    "  Run audit_session() to check for AI data exposure risks.\n",
    "  Run confidential_mode_on() before loading confidential data."
  )

  # Note: this function only detects stale backups *within the same R session*.
  # When R is closed, options() are discarded, so there's nothing to clean up
  # across sessions — keys set via Sys.setenv() are simply lost, and keys in
  # .Renviron reload normally. The message below is defensive against the
  # case where safecatch is detached and re-loaded in the same session.
  stale_backups <- character(0)
  for (key in .safecatch_api_keys) {
    opt_name <- paste0("safecatch.backup.", tolower(gsub("[^A-Za-z0-9]", "_", key)))
    val <- getOption(opt_name, NULL)
    if (!is.null(val) && nzchar(val)) {
      stale_backups <- c(stale_backups, key)
    }
  }
  if (length(stale_backups) > 0 && !isTRUE(getOption("safecatch.confidential_mode", FALSE))) {
    packageStartupMessage(
      "  NOTE: found ", length(stale_backups), " backed-up API key(s) from a previous\n",
      "  confidential session that was not closed cleanly. Call restore_api_keys()\n",
      "  to restore them, or api_key_status() to inspect."
    )
  }
}
