#' Perform a REST POST request against Hedera services
#'
#' Execute a POST request using the REST transport configuration.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param path Endpoint path relative to the REST base URL, or an absolute URL.
#' @param body Optional named list to send as a JSON payload.
#'
#' @return A parsed JSON response as a list.
#'
#' @examples
#' config <- hadeda_config(
#'   rest = list(base_url = "https://testnet.hashio.io/api/v1")
#' )
#' payload <- list(memo = "demo")
#' \dontrun{
#' hadeda_rest_post(config, "accounts", payload)
#' }
#'
#' @keywords internal
hadeda_rest_post <- function(config, path, body = list()) {
  rest <- hadeda_require_rest(config)
  base <- rest$base_url

  req <- if (grepl("^https?://", path)) {
    httr2::request(path)
  } else {
    httr2::request(base) |>
      httr2::req_url_path_append(path)
  }

  headers <- rest$headers %||% list()
  if (length(headers) > 0) {
    req <- rlang::exec(httr2::req_headers, req, !!!headers)
  }

  if (length(body) > 0) {
    req <- httr2::req_body_json(req, body)
  }

  req <- httr2::req_method(req, "POST")
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
