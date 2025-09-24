#' Ensure a REST configuration is present
#'
#' Validate that a configuration includes REST settings.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param caller Environment used for error messaging.
#'
#' @return The REST configuration list.
#'
#' @examples
#' config <- hadeda_config()
#' hadeda_require_rest(config)
#'
#' @keywords internal
hadeda_require_rest <- function(config, caller = rlang::caller_env()) {
  rest <- config$rest
  if (is.null(rest) || !is.list(rest) || is.null(rest$base_url)) {
    cli::cli_abort(
      "REST configuration is missing a `base_url`.\nDid you create the configuration with `hadeda_config()`?",
      class = "hadeda_missing_rest_config",
      call = caller
    )
  }
  rest
}
