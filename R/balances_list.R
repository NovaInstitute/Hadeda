#' List account balances at a consensus timestamp
#'
#' Retrieve aggregated account balances from the Hedera Mirror Node REST API.
#' The endpoint returns a snapshot of all accounts at a consensus timestamp and
#' supports filtering by account ID, balance thresholds, public keys, and
#' ordering options.
#'
#' @param config A configuration list created by [hadeda_config()].
#' @param limit Optional page size for the Mirror Node request.
#' @param account_id Optional account ID filter.
#' @param balance Optional balance filter expressed in tinybars for equality
#'   matching.
#' @param balance_min Optional minimum balance filter (in tinybars).
#' @param balance_max Optional maximum balance filter (in tinybars).
#' @param public_key Optional public key filter.
#' @param timestamp Optional consensus timestamp filter. Accepts comparison
#'   operators such as `"gt:"`, `"lt:"`, and `"eq:"` prefixes supported by the
#'   Mirror Node API.
#' @param order Result ordering, either "desc" or "asc".
#' @param .transport Optional transport override ("rest" or "grpc").
#'
#' @return A tibble containing account balances at the requested timestamp.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   balances_list(
#'     mirror,
#'     limit = 25,
#'     timestamp = "lt:1700000000.000000000",
#'     balance_min = 1e8
#'   )
#' }
#'
#' @export
balances_list <- function(config,
                          limit = NULL,
                          account_id = NULL,
                          balance = NULL,
                          balance_min = NULL,
                          balance_max = NULL,
                          public_key = NULL,
                          timestamp = NULL,
                          order = c("desc", "asc"),
                          .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = TRUE,
    grpc_supported = FALSE
  )
  if (!identical(transport, "rest")) {
    cli::cli_abort("balances_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    `account.id` = account_id,
    `account.balance` = balance,
    `account.balance.gt` = balance_min,
    `account.balance.lt` = balance_max,
    publickey = public_key,
    timestamp = timestamp,
    order = match.arg(order)
  ))

  responses <- hadeda_rest_paginate(config, "balances", query)
  tables <- purrr::map(responses, hadeda_parse_account_balances)

  if (length(tables) == 0) {
    return(hadeda_parse_account_balances(list(balances = list())))
  }

  vctrs::vec_rbind(!!!tables)
}
