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
#' config <- hadeda_config()
#' account_id <- "0.0.1001"
#' account <- accounts_get(config = config, account_id = account_id)
#' account
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
