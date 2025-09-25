#' Retrieve consensus topic metadata via gRPC
#'
#' Query the Hedera `getTopicInfo` RPC to fetch consensus topic metadata using
#' the configured gRPC transport.
#'
#' @inheritParams topics_get
#'
#' @return A tibble with a single row describing the requested topic. The raw
#'   gRPC response is returned in a `response` list-column for downstream
#'   inspection.
#'
#' @export
consensus_topic_info <- function(config,
                                 topic_id,
                                 .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("consensus_topic_info() currently supports only the gRPC transport.")
  }

  if (missing(topic_id) || is.null(topic_id) || identical(topic_id, "")) {
    cli::cli_abort("`topic_id` is required to request topic info.")
  }

  response <- hadeda_grpc_consensus_topic_info(
    config = config,
    topic_id = topic_id
  )

  record <- response$topic_info %||% response$topicInfo %||% response
  hadeda_parse_topics(list(record)) |>
    tibble::add_column(response = list(response), .after = "topic_id")
}

#' @keywords internal
hadeda_grpc_consensus_topic_info <- function(config, topic_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_topic_info %||% grpc$consensus_get_topic_info %||% grpc$topic_info
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC ConsensusService topic info handler configured.\nProvide `config$grpc$get_topic_info` to enable topic info queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    topic_id = topic_id
  )
}
