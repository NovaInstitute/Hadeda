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

hadeda_rest_paginate <- function(config, path, query = list()) {
  responses <- list()
  next_path <- path
  next_query <- query

  while (!is.null(next_path)) {
    resp <- hadeda_rest_get(config, next_path, next_query)
    responses <- append(responses, list(resp))

    links <- resp$links %||% list()
    next_link <- links$next %||% NULL
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
