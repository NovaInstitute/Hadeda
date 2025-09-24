#' Retrieve a single Hedera transaction
#'
#' Fetch metadata for a single transaction by identifier.
#'
#' @inheritParams transactions_list
#' @param transaction_id The transaction identifier.
#'
#' @return A tibble with a single row describing the transaction.
#'
#' @examples
#' config <- hadeda_config()
#' transaction_id <- "0.0.1001-1700000000-000000000"
#' transaction <- transactions_get(config, transaction_id)
#' transaction
#'
#' @export
transactions_get <- function(config, transaction_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("transactions_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("transactions/", transaction_id))
  hadeda_parse_transactions(list(resp))
}
