#' List Hedera network nodes
#'
#' Retrieve node metadata from the Hedera Mirror Node REST API.
#'
#' @inheritParams accounts_list
#' @param limit Optional maximum number of nodes to return.
#' @param node_id Optional node identifier filter.
#'
#' @return A tibble describing network nodes.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   network_nodes(mirror)
#' }
#'
#' @export
network_nodes <- function(config,
                          limit = NULL,
                          node_id = NULL,
                          order = c("asc", "desc"),
                          .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("network_nodes() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    `node.id` = node_id
  ))

  responses <- hadeda_rest_paginate(config, "network/nodes", query)
  records <- purrr::map(responses, "nodes")
  records <- purrr::flatten(records)
  hadeda_parse_network_nodes(records)
}
