#' List Hedera accounts
#'
#' Retrieve account summaries from the Hedera Mirror Node REST API.
#'
#' @param config A configuration list created by [hadeda_config()].
#' @param limit Optional page size for the Mirror Node request.
#' @param account_id Optional account ID filter.
#' @param public_key Optional public key filter.
#' @param balance_min Optional minimum balance filter (in tinybars).
#' @param balance_max Optional maximum balance filter (in tinybars).
#' @param order Result ordering, either "asc" or "desc".
#' @param .transport Optional transport override ("rest" or "grpc").
#'
#' @return A tibble of accounts with balance metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' hashio <- hadeda_config(
#'   network = "testnet",
#'   rest = list(
#'     base_url = "https://testnet.hashio.io/api/v1",
#'     headers = list(`X-API-Key` = Sys.getenv("HASHIO_API_KEY"))
#'   ),
#'   default_transport = "rest"
#' )
#' \dontrun{
#'   new_account <- accounts_create(hashio)
#'   accounts_list(mirror, account_id = new_account$account)
#' }
#'
#' @export
accounts_list <- function(config,
                          limit = NULL,
                          account_id = NULL,
                          public_key = NULL,
                          balance_min = NULL,
                          balance_max = NULL,
                          order = c("desc", "asc"),
                          .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    account.id = account_id,
    publickey = public_key,
    balance.min = balance_min,
    balance.max = balance_max,
    order = match.arg(order)
  ))

  responses <- hadeda_rest_paginate(config, "accounts", query)
  records <- purrr::map(responses, "accounts")
  records <- purrr::flatten(records)
  hadeda_parse_accounts(records)
}
