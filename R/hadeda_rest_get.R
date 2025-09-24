#' Perform a REST request against the Mirror Node
#'
#' Execute a GET request using the REST transport configuration.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param path Endpoint path relative to the REST base URL, or an absolute URL.
#' @param query Optional named list of query parameters.
#'
#' @return A parsed JSON response as a list.
#'
#' @examples
#' config <- hadeda_config()
#' path <- "accounts"
#' query <- list(limit = 5)
#' \dontrun{
#' hadeda_rest_get(config, path, query)
#' }
#'
#' @keywords internal
hadeda_rest_get <- function(config, path, query = list()) {
  rest <- hadeda_require_rest(config)
  base <- rest$base_url

  req <- if (grepl("^https?://", path)) {
    httr2::request(path)
  } else {
    httr2::request(base) |>
      httr2::req_url_path_append(path)
  }

  if (length(query) > 0) {
    req <- rlang::exec(httr2::req_url_query, req, !!!query)
  }

  headers <- rest$headers %||% list()
  if (length(headers) > 0) {
    req <- rlang::exec(httr2::req_headers, req, !!!headers)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}
