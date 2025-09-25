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
#'   accounts_balance(
#'     config = mirror,
#'     account_id = new_account$account,
#'     timestamp = "1700000000.000000000"
#'   )
#' }
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
