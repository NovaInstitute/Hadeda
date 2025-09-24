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
#' config <- hadeda_config()
#' token_id <- "0.0.1001"
#' token <- tokens_get(config, token_id)
#' token
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
