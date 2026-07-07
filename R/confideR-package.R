#' confideR: Protect Confidential Research Data When Using AI Coding Assistants
#'
#' Tools to protect confidential research data during AI-assisted code
#' development. Enforces a confidential mode that clears AI API keys and
#' blocks AI-adjacent R packages, audits the session for active AI features,
#' generates privacy-safe structural fingerprints of real datasets, and
#' simulates realistic survey data for safe use with AI coding assistants.
#'
#' Run \code{\link{audit_session}()} to check for AI data exposure risks, then
#' \code{\link{confidential_mode_on}()} before loading confidential data.
#'
#' @keywords internal
#' @importFrom stats quantile sd rnorm rlnorm runif rbinom
#' @importFrom utils packageVersion
"_PACKAGE"
