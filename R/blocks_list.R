#' List Hedera blocks
#'
#' Retrieve block summaries from the Hedera Mirror Node REST API.
#'
#' @inheritParams accounts_list
#' @param block_hash Optional block hash filter.
#' @param block_number Optional block number filter.
#' @param timestamp Optional timestamp filter expression such as "gte:1670000000.000000000".
#'
#' @return A tibble of block summaries.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   blocks_list(mirror, limit = 5)
#' }
#'
#' @export
blocks_list <- function(config,
                        limit = NULL,
                        order = c("desc", "asc"),
                        block_hash = NULL,
                        block_number = NULL,
                        timestamp = NULL,
                        .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("blocks_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    `block.hash` = block_hash,
    `block.number` = block_number,
    timestamp = timestamp
  ))

  responses <- hadeda_rest_paginate(config, "blocks", query)
  records <- purrr::map(responses, "blocks")
  records <- purrr::flatten(records)
  hadeda_parse_blocks(records)
}
