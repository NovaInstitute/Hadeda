#' Merge default and user-supplied transport configuration
#'
#' Combine default transport options with user overrides.
#'
#' @param defaults A list providing the baseline configuration for a transport.
#' @param override An optional list of overrides supplied by the user.
#'
#' @return A list with the same structure as `defaults` but with overrides applied.
#'
#' @examples
#' defaults <- list(base_url = "https://example.com", rate_limit = list(requests_per_second = 10))
#' override <- list(rate_limit = list(requests_per_second = 5))
#' hadeda_merge_config(defaults, override)
#'
#' @export
hadeda_merge_config <- function(defaults = NULL, override = NULL) {
  if (is.null(defaults) && is.null(override)) {
    return(list())
  }
  if (!is.null(defaults) && !is.list(defaults)) {
    cli::cli_abort("`defaults` must be a list or NULL")
  }
  if (!is.null(override) && !is.list(override)) {
    cli::cli_abort("`override` must be a list or NULL")
  }

  defaults <- defaults %||% list()
  override <- override %||% list()
  utils::modifyList(defaults, override, keep.null = TRUE)
}
