#' Retrieve consensus topic metadata
#'
#' Fetch metadata for a specific consensus topic using the Mirror Node
#' REST API.
#'
#' @inheritParams topics_messages
#' @param topic_id Consensus topic identifier.
#'
#' @return A tibble with a single row describing the requested topic.
#'
#' @export
topics_get <- function(config,
                       topic_id,
                       .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("topics_get() currently supports only the REST transport.")
  }

  if (missing(topic_id) || is.null(topic_id)) {
    cli::cli_abort("topic_id is required to retrieve a topic.")
  }

  resp <- hadeda_rest_get(config, paste0("topics/", topic_id))
  record <- resp[["topic"]] %||% resp

  if (is.null(record) || length(record) == 0) {
    return(hadeda_parse_topics(list()))
  }

  while (
    is.list(record) &&
      length(record) == 1 &&
      is.list(record[[1]]) &&
      !rlang::has_name(record, "topic_id") &&
      !rlang::has_name(record, "topicId")
  ) {
    record <- record[[1]]
  }

  hadeda_parse_topics(list(record))
}
