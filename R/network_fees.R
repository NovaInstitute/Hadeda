#' Retrieve Hedera network fee schedules
#'
#' Fetch the current and next transaction fee schedules from the Mirror Node REST API.
#'
#' @inheritParams accounts_list
#'
#' @return A tibble summarising fee schedule periods with nested fee data.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   network_fees(mirror)
#' }
#'
#' @export
network_fees <- function(config, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("network_fees() currently supports only the REST transport.")
  }

  response <- hadeda_rest_get(config, "network/fees")
  hadeda_parse_network_fees(response)
}

