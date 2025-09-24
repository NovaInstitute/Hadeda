#' Paginate through Mirror Node REST responses
#'
#' Iterate through paginated endpoints, accumulating responses.
#'
#' @inheritParams hadeda_rest_get
#'
#' @return A list of parsed responses for each page.
#'
#' @examples
#' config <- hadeda_config()
#' path <- "transactions"
#' query <- list(limit = 2)
#' \dontrun{
#' hadeda_rest_paginate(config, path, query)
#' }
#'
#' @keywords internal
hadeda_rest_paginate <- function(config, path, query = list()) {
  responses <- list()
  next_path <- path
  next_query <- query

  while (!is.null(next_path)) {
    resp <- hadeda_rest_get(config, next_path, next_query)
    responses <- append(responses, list(resp))

    links <- resp$links %||% list()
    next_link <- links[["next"]] %||% NULL
    if (is.null(next_link) || identical(next_link, "")) {
      break
    }

    if (grepl("^https?://", next_link)) {
      next_path <- next_link
      next_query <- list()
    } else {
      pieces <- httr2::url_parse(next_link)
      next_path <- paste(pieces$path, collapse = "/")
      next_query <- hadeda_drop_null(pieces$query)
    }
  }

  responses
}
