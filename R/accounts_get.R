#' Retrieve a single Hedera account
#'
#' Fetch account metadata for a specific account identifier.
#'
#' @inheritParams accounts_list
#' @param account_id The account identifier to retrieve.
#'
#' @return A tibble with a single row describing the account.
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
#'   accounts_get(mirror, new_account$account)
#' }
#'
#' @export
accounts_get <- function(config, account_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("accounts_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("accounts/", account_id))
  hadeda_parse_accounts(list(resp))
}
