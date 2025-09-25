#' List approved NFT allowances for an account
#'
#' Retrieve NFT allowances granted by an account via the Hedera Mirror Node REST API.
#'
#' @param config A configuration list created by [hadeda_config()].
#' @param account_id The account whose NFT allowances should be retrieved.
#' @param limit Optional page size for the Mirror Node request.
#' @param spender_id Optional filter restricting results to a specific spender account ID.
#' @param token_id Optional filter restricting results to a specific token ID.
#' @param order Result ordering, either "desc" or "asc".
#' @param timestamp Optional consensus timestamp filter (for example "gt:1672531300.000000000Z").
#' @param .transport Optional transport override ("rest" or "grpc").
#'
#' @return A tibble of NFT allowances granted by the account.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   accounts_allowances_nfts(
#'     mirror,
#'     account_id = "0.0.1234",
#'     token_id = "0.0.5678",
#'     spender_id = "0.0.9876"
#'   )
#' }
#'
#' @export
accounts_allowances_nfts <- function(config,
                                      account_id,
                                      limit = NULL,
                                      spender_id = NULL,
                                      token_id = NULL,
                                      order = c("desc", "asc"),
                                      timestamp = NULL,
                                      .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_allowances_nfts() currently supports only the REST transport.")
  }

  if (missing(account_id) || is.null(account_id)) {
    cli::cli_abort("`account_id` must be supplied to accounts_allowances_nfts().")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    `spender.id` = spender_id,
    `token.id` = token_id,
    timestamp = timestamp
  ))

  path <- paste0("accounts/", account_id, "/allowances/nfts")
  responses <- hadeda_rest_paginate(config, path, query)
  records <- purrr::map(responses, "allowances")
  records <- purrr::flatten(records)
  hadeda_parse_account_allowances_nfts(records)
}
