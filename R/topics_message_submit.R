#' Submit a message to a consensus topic
#'
#' Publish a message to an existing topic over the REST transport.
#'
#' @inheritParams topics_messages
#' @param topic_id Consensus topic identifier.
#' @param message Message payload to submit. When `message_type` is "text"
#'   the value will be UTF-8 encoded before transmission.
#' @param message_type Indicates whether `message` is already base64 encoded
#'   or should be treated as plain text.
#' @param memo Optional message memo.
#' @param chunk_info Optional chunking metadata as a named list.
#'
#' @return A tibble containing submission details including the assigned
#'   sequence number and consensus timestamp.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   topics_message_submit(
#'     mirror,
#'     topic_id = "0.0.4001",
#'     message = "Hello from Hadeda",
#'     message_type = "text"
#'   )
#' }
#'
#' @export
topics_message_submit <- function(config,
                                  topic_id,
                                  message,
                                  message_type = c("base64", "text"),
                                  memo = NULL,
                                  chunk_info = NULL,
                                  .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("topics_message_submit() currently supports only the REST transport.")
  }

  if (missing(topic_id) || is.null(topic_id)) {
    cli::cli_abort("topic_id is required to submit a message.")
  }
  if (missing(message) || is.null(message)) {
    cli::cli_abort("message is required to submit a topic message.")
  }

  message_type <- rlang::arg_match(message_type)
  payload <- message
  if (identical(message_type, "text")) {
    payload <- jsonlite::base64_enc(charToRaw(as.character(message)))
  }

  body <- hadeda_drop_null(list(
    message = payload,
    memo = memo,
    chunkInfo = chunk_info
  ))

  resp <- hadeda_rest_post(config, paste0("topics/", topic_id, "/messages"), body)

  status <- resp$status %||% resp$receipt$status %||% NA_character_
  sequence_number <- resp$sequenceNumber %||% resp$sequence_number %||% NA_real_
  consensus_raw <- resp$consensusTimestamp %||% resp$consensus_timestamp
  consensus_timestamp <- if (is.null(consensus_raw)) NA_character_ else as.character(consensus_raw)

  tibble::tibble(
    topic_id = as.character(topic_id),
    sequence_number = if (is.null(sequence_number)) NA_real_ else as.numeric(sequence_number),
    consensus_timestamp = hadeda_parse_timestamp(consensus_timestamp),
    status = if (is.null(status)) NA_character_ else as.character(status),
    response = list(resp)
  )
}
