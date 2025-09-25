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
  split_path_segments <- function(path) {
    if (is.null(path) || length(path) == 0) {
      return(character())
    }

    path <- path[[1]]
    segments <- strsplit(path, "/", fixed = TRUE)[[1]]
    segments[segments != ""]
  }

  rest <- hadeda_require_rest(config)
  base_segments <- split_path_segments(httr2::url_parse(rest$base_url)$path)

  responses <- list()
  next_path <- path
  next_query <- query

  while (!is.null(next_path)) {
    current_path <- next_path
    current_query <- next_query
    resp <- hadeda_rest_get(config, current_path, current_query)
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
      relative <- sub("^/+", "", next_link)
      pieces <- httr2::url_parse(paste0("https://placeholder.invalid/", relative))

      path_segments <- split_path_segments(pieces$path)
      if (length(base_segments) > 0 &&
          length(path_segments) >= length(base_segments) &&
          identical(path_segments[seq_along(base_segments)], base_segments)) {
        path_segments <- path_segments[-seq_along(base_segments)]
      }

      next_path <- paste(path_segments, collapse = "/")
      if (identical(next_path, "")) {
        next_path <- current_path
      }

      next_query <- hadeda_drop_null(pieces$query %||% list())
    }
  }

  responses
}
