#' List approved HBAR allowances for an account
#'
#' Retrieve crypto allowances granted by an account via the Hedera Mirror Node REST API.
#'
#' @param config A configuration list created by [hadeda_config()].
#' @param account_id The account whose allowances should be retrieved.
#' @param limit Optional page size for the Mirror Node request.
#' @param spender_id Optional filter restricting results to a specific spender account ID.
#' @param order Result ordering, either "desc" or "asc".
#' @param timestamp Optional consensus timestamp filter (for example "gt:1672531300.000000000Z").
#' @param .transport Optional transport override ("rest" or "grpc").
#'
#' @return A tibble of crypto allowances granted by the account.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   accounts_allowances_crypto(
#'     mirror,
#'     account_id = "0.0.1234",
#'     limit = 25,
#'     spender_id = "0.0.5678"
#'   )
#' }
#'
#' @export
accounts_allowances_crypto <- function(config,
                                        account_id,
                                        limit = NULL,
                                        spender_id = NULL,
                                        order = c("desc", "asc"),
                                        timestamp = NULL,
                                        .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_allowances_crypto() currently supports only the REST transport.")
  }

  if (missing(account_id) || is.null(account_id)) {
    cli::cli_abort("`account_id` must be supplied to accounts_allowances_crypto().")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    `spender.id` = spender_id,
    timestamp = timestamp
  ))

  path <- paste0("accounts/", account_id, "/allowances/crypto")
  responses <- hadeda_rest_paginate(config, path, query)
  records <- purrr::map(responses, "allowances")
  records <- purrr::flatten(records)
  hadeda_parse_account_allowances_crypto(records)
}
