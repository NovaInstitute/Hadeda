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
#'   new_token <- tokens_create(
#'     hashio,
#'     name = "Hadeda Example",
#'     symbol = "HADEDA",
#'     treasury_account_id = new_account$account
#'   )
#'   transactions_get(mirror, new_token$transaction_id)
#' }
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
