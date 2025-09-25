#' Retrieve token metadata
#'
#' Fetch token details from the Mirror Node.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param token_id Token identifier.
#' @param .transport Optional transport override.
#'
#' @return A tibble describing the token.
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
#'   tokens_get(mirror, new_token$token_id)
#' }
#'
#' @export
tokens_get <- function(config, token_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("tokens/", token_id))
  hadeda_parse_tokens(list(resp))
}
