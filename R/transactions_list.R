#' List Hedera transactions
#'
#' Retrieve transactions from the Mirror Node with optional filters.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param limit Optional page size.
#' @param order Ordering of results, either "desc" or "asc".
#' @param transaction_type Optional transaction type filter.
#' @param account_id Optional account ID filter.
#' @param timestamp Optional timestamp range filter.
#' @param .transport Optional transport override.
#'
#' @return A tibble of transactions.
#'
#' @examples
#' config <- hadeda_config()
#' transactions <- transactions_list(
#'   config = config,
#'   limit = 10,
#'   transaction_type = "CRYPTOTRANSFER"
#' )
#' transactions
#'
#' @export
transactions_list <- function(config,
                              limit = NULL,
                              order = c("desc", "asc"),
                              transaction_type = NULL,
                              account_id = NULL,
                              timestamp = NULL,
                              .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("transactions_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    transactiontype = transaction_type,
    account.id = account_id,
    timestamp = timestamp
  ))

  responses <- hadeda_rest_paginate(config, "transactions", query)
  records <- purrr::map(responses, "transactions")
  records <- purrr::flatten(records)
  hadeda_parse_transactions(records)
}
