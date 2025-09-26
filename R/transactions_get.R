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
#'   transactions_get(mirror, "0.0.1234-1700000000-000000000")
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
