#' Retrieve Hedera network supply metrics
#'
#' Fetch current HBAR supply metrics from the Mirror Node REST API.
#'
#' @inheritParams accounts_list
#'
#' @return A tibble describing supply statistics for the Hedera network.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   network_supply(mirror)
#' }
#'
#' @export
network_supply <- function(config, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("network_supply() currently supports only the REST transport.")
  }

  response <- hadeda_rest_get(config, "network/supply")
  hadeda_parse_network_supply(response)
}

