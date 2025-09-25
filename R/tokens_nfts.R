#' List token NFTs
#'
#' Retrieve non-fungible token metadata for a specific token.
#'
#' @inheritParams tokens_get
#' @param token_id Token identifier to query.
#' @param limit Optional page size.
#' @param order Result order, either "asc" or "desc".
#' @param account_id Optional owner account filter.
#' @param spender_id Optional spender account filter.
#' @param serial_number Optional serial number filter.
#'
#' @return A tibble describing NFTs for the token.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   tokens_nfts(mirror, "0.0.1001", limit = 10)
#' }
#'
#' @export
tokens_nfts <- function(config,
                         token_id,
                         limit = NULL,
                         order = c("desc", "asc"),
                         account_id = NULL,
                         spender_id = NULL,
                         serial_number = NULL,
                         .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_nfts() currently supports only the REST transport.")
  }

  if (missing(token_id)) {
    cli::cli_abort("token_id is required to query NFTs.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    `account.id` = account_id,
    `spender.id` = spender_id,
    serialnumber = serial_number
  ))

  path <- sprintf("tokens/%s/nfts", token_id)
  responses <- hadeda_rest_paginate(config, path, query)
  records <- purrr::map(responses, "nfts")
  records <- purrr::flatten(records)
  hadeda_parse_token_nfts(records)
}

#' Retrieve a specific token NFT
#'
#' @inheritParams tokens_nfts
#' @param serial_number Serial number of the NFT.
#'
#' @return A tibble with a single NFT record.
#'
#' @export
tokens_nft <- function(config,
                        token_id,
                        serial_number,
                        .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_nft() currently supports only the REST transport.")
  }

  if (missing(token_id) || missing(serial_number)) {
    cli::cli_abort("token_id and serial_number are required to fetch an NFT.")
  }

  path <- sprintf("tokens/%s/nfts/%s", token_id, serial_number)
  resp <- hadeda_rest_get(config, path)
  record <- resp$nft %||% resp
  tbl <- hadeda_parse_token_nfts(list(record))
  if (nrow(tbl) == 0) {
    cli::cli_abort("NFT response did not contain a record.")
  }
  tibble::as_tibble(tbl[1, , drop = FALSE])
}
