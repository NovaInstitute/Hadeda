#' Retrieve the balance for an account
#'
#' Fetch token balances for an account at an optional consensus timestamp.
#'
#' @inheritParams accounts_list
#' @param account_id The account identifier to retrieve.
#' @param timestamp Optional consensus timestamp filter.
#'
#' @return A tibble with balance and token holdings.
#'
#' @examples
#' config <- hadeda_config()
#' balance <- accounts_balance(
#'   config = config,
#'   account_id = "0.0.1001",
#'   timestamp = "1700000000.000000000"
#' )
#' balance
#'
#' @export
accounts_balance <- function(config, account_id, timestamp = NULL, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_balance() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(timestamp = timestamp))
  resp <- hadeda_rest_get(config, paste0("accounts/", account_id, "/balance"), query)
  hadeda_parse_account_balances(resp)
}
