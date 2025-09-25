#' Retrieve Hedera network stake information
#'
#' Fetch aggregated staking metrics from the Mirror Node REST API.
#'
#' @inheritParams accounts_list
#' @param limit Optional maximum number of stake snapshots to retrieve.
#' @param order Sort order for the snapshots.
#'
#' @return A tibble describing staking snapshots.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   network_stake(mirror)
#' }
#'
#' @export
network_stake <- function(config,
                          limit = NULL,
                          order = c("desc", "asc"),
                          .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("network_stake() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order)
  ))

  responses <- hadeda_rest_paginate(config, "network/stake", query)
  records <- purrr::map(responses, "stakes")
  records <- purrr::flatten(records)
  hadeda_parse_network_stake(records)
}
