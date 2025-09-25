#' Retrieve Hedera network exchange rates
#'
#' Fetch the current and next HBAR to USD cent exchange rates from the Mirror Node REST API.
#'
#' @inheritParams accounts_list
#'
#' @return A tibble describing the current and next exchange rates.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   network_exchange_rate(mirror)
#' }
#'
#' @export
network_exchange_rate <- function(config, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("network_exchange_rate() currently supports only the REST transport.")
  }

  response <- hadeda_rest_get(config, "network/exchangerate")
  current_rate <- response$current_rate %||% response$currentRate
  next_rate <- response$next_rate %||% response$nextRate

  records <- purrr::compact(list(
    if (!rlang::is_null(current_rate)) list(type = "current", rate = current_rate) else NULL,
    if (!rlang::is_null(next_rate)) list(type = "next", rate = next_rate) else NULL
  ))

  hadeda_parse_network_exchange_rate(records, response$timestamp)
}
