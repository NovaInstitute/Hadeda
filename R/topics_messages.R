#' Retrieve messages for a consensus topic
#'
#' Fetch consensus messages for a topic from the Mirror Node.
#'
#' @inheritParams transactions_list
#' @param topic_id Consensus topic identifier.
#' @param sequencenumber Optional sequence number filter.
#'
#' @return A tibble of topic messages.
#'
#' @examples
#' config <- hadeda_config()
#' topic_id <- "0.0.2002"
#' messages <- topics_messages(
#'   config = config,
#'   topic_id = topic_id,
#'   limit = 10
#' )
#' messages
#'
#' @export
topics_messages <- function(config,
                            topic_id,
                            limit = NULL,
                            order = c("desc", "asc"),
                            sequencenumber = NULL,
                            timestamp = NULL,
                            .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("topics_messages() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    sequencenumber = sequencenumber,
    timestamp = timestamp
  ))

  responses <- hadeda_rest_paginate(config, paste0("topics/", topic_id, "/messages"), query)
  records <- purrr::map(responses, "messages")
  records <- purrr::flatten(records)
  hadeda_parse_topic_messages(records)
}
