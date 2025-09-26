#' Retrieve token balance information
#'
#' Fetch account balances for a token from the Mirror Node.
#'
#' @inheritParams tokens_get
#' @param limit Optional page size.
#' @param account_id Optional filter restricting balances to a specific account.
#' @param order Ordering of results, either "desc" or "asc".
#'
#' @return A tibble of token balances.
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
#'   tokens_balances(
#'     config = mirror,
#'     token_id = "0.0.7007",
#'     limit = 10,
#'     account_id = "0.0.1234"
#'   )
#' }
#'
#' @export
tokens_balances <- function(config,
                            token_id,
                            limit = NULL,
                            account_id = NULL,
                            order = c("desc", "asc"),
                            .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("tokens_balances() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    account.id = account_id,
    order = match.arg(order)
  ))

  responses <- hadeda_rest_paginate(config, paste0("tokens/", token_id, "/balances"), query)
  records <- purrr::map(responses, "balances")
  records <- purrr::flatten(records)
  hadeda_parse_token_balances(records)
}
