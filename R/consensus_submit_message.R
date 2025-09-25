#' Submit a message via the Consensus Service
#'
#' Publish a message to a Hedera consensus topic using the gRPC transport.
#'
#' @inheritParams topics_message_submit
#' @param message_type Indicates the input format for `message`. Accepted
#'   values are "text", "raw", or "base64".
#' @param chunk_size Maximum message payload per chunk in bytes. Defaults to
#'   6,144 bytes to mirror Hedera consensus limits.
#' @param wait_for_receipt When `TRUE`, the helper expects the transport layer
#'   to resolve a transaction receipt acknowledging submission.
#'
#' @return A tibble summarising acknowledgement details for each submitted
#'   chunk. The tibble includes the chunk number, transaction identifier,
#'   precheck status, receipt status, consensus timestamp, and the raw transport
#'   response.
#'
#' @export
consensus_submit_message <- function(config,
                                     topic_id,
                                     message,
                                     message_type = c("text", "raw", "base64"),
                                     memo = NULL,
                                     chunk_size = 6144L,
                                     wait_for_receipt = TRUE,
                                     .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("consensus_submit_message() currently supports only the gRPC transport.")
  }

  if (missing(topic_id) || is.null(topic_id) || identical(topic_id, "")) {
    cli::cli_abort("`topic_id` is required to submit a consensus message.")
  }
  if (missing(message) || is.null(message)) {
    cli::cli_abort("`message` is required to submit a consensus message.")
  }

  chunk_size <- as.integer(chunk_size[[1]])
  if (!is.finite(chunk_size) || chunk_size <= 0) {
    cli::cli_abort("`chunk_size` must be a positive integer number of bytes.")
  }

  message_type <- rlang::arg_match(message_type)
  payload <- hadeda_normalise_message_payload(message, message_type)

  chunks <- hadeda_chunk_payload(payload, chunk_size)
  total_chunks <- length(chunks)
  initial_transaction_id <- NULL
  if (total_chunks > 1L) {
    initial_transaction_id <- hadeda_generate_transaction_id(config)
  }

  responses <- vector("list", total_chunks)
  for (idx in seq_along(chunks)) {
    chunk_info <- NULL
    if (total_chunks > 1L) {
      chunk_info <- list(
        number = idx,
        total = total_chunks,
        initial_transaction_id = initial_transaction_id
      )
    }
    responses[[idx]] <- hadeda_grpc_consensus_submit_message(
      config = config,
      topic_id = topic_id,
      payload = chunks[[idx]],
      memo = memo,
      chunk_info = chunk_info,
      wait_for_receipt = wait_for_receipt
    )
  }

  hadeda_parse_consensus_responses(topic_id, responses)
}

#' @keywords internal
hadeda_normalise_message_payload <- function(message, message_type) {
  if (identical(message_type, "text")) {
    return(charToRaw(paste0(message, collapse = "")))
  }
  if (identical(message_type, "raw")) {
    if (!is.raw(message)) {
      cli::cli_abort("When `message_type` is 'raw' the `message` argument must be a raw vector.")
    }
    return(message)
  }

  # base64 branch
  if (rlang::is_empty(message) || is.null(message)) {
    cli::cli_abort("Base64 payloads must be supplied as a non-empty character string.")
  }
  encoded <- as.character(message[[1]])
  jsonlite::base64_dec(encoded)
}

#' @keywords internal
hadeda_chunk_payload <- function(payload, chunk_size) {
  if (!is.raw(payload)) {
    cli::cli_abort("Payload must be provided as a raw vector.")
  }
  if (length(payload) == 0) {
    return(list(raw(0)))
  }
  starts <- seq.int(1L, length(payload), by = chunk_size)
  lapply(starts, function(start) {
    end <- min(start + chunk_size - 1L, length(payload))
    payload[start:end]
  })
}

#' @keywords internal
hadeda_generate_transaction_id <- function(config) {
  grpc <- hadeda_require_grpc(config)
  operator <- grpc$operator_account_id %||% grpc$operator_id %||% "0.0.0"
  now <- as.numeric(lubridate::now(tzone = "UTC"))
  seconds <- floor(now)
  nanos <- round((now - seconds) * 1e9)
  sprintf("%s-%s-%09d", operator, seconds, nanos)
}

#' @keywords internal
hadeda_grpc_consensus_submit_message <- function(config,
                                                 topic_id,
                                                 payload,
                                                 memo = NULL,
                                                 chunk_info = NULL,
                                                 wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$submit_message
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC submission handler is configured.\nProvide `config$grpc$submit_message` to enable consensus message submission.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    topic_id = topic_id,
    payload = payload,
    memo = memo,
    chunk_info = chunk_info,
    wait_for_receipt = wait_for_receipt
  )
}

#' @keywords internal
hadeda_parse_consensus_responses <- function(topic_id, responses) {
  if (length(responses) == 0) {
    return(tibble::tibble(
      topic_id = character(),
      chunk_number = integer(),
      transaction_id = character(),
      status = character(),
      receipt_status = character(),
      acknowledged = logical(),
      sequence_number = numeric(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      response = list()
    ))
  }

  chunk_numbers <- seq_along(responses)

  extract_transaction_id <- function(resp) {
    tx <- resp$transaction_id %||% resp$transactionId %||% resp$transactionID
    if (is.null(tx)) NA_character_ else as.character(tx)
  }
  extract_status <- function(resp) {
    status <- resp$status %||% resp$precheck_code %||% resp$node_transaction_precheck_code %||% resp$nodeTransactionPrecheckCode
    if (is.null(status)) NA_character_ else as.character(status)
  }
  extract_receipt <- function(resp) {
    receipt <- resp$receipt %||% resp$transaction_receipt %||% resp$transactionReceipt
    if (is.null(receipt)) list() else receipt
  }
  extract_receipt_status <- function(receipt) {
    status <- receipt$status %||% receipt$receipt_status %||% receipt$receiptStatus
    if (is.null(status)) NA_character_ else as.character(status)
  }
  extract_sequence_number <- function(resp, receipt) {
    seq_val <- resp$sequence_number %||% resp$sequenceNumber %||% receipt$sequence_number %||% receipt$sequenceNumber
    if (is.null(seq_val)) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(seq_val))
  }
  extract_timestamp <- function(resp, receipt) {
    ts <- resp$consensus_timestamp %||% resp$consensusTimestamp %||% receipt$consensus_timestamp %||% receipt$consensusTimestamp
    if (is.null(ts)) {
      return(lubridate::as_datetime(NA_real_))
    }
    hadeda_parse_timestamp(as.character(ts))
  }

  receipt_list <- lapply(responses, extract_receipt)
  receipt_status <- vapply(receipt_list, extract_receipt_status, character(1), USE.NAMES = FALSE)
  status <- vapply(responses, extract_status, character(1), USE.NAMES = FALSE)
  sequence <- vapply(seq_along(responses), function(idx) {
    extract_sequence_number(responses[[idx]], receipt_list[[idx]])
  }, numeric(1), USE.NAMES = FALSE)
  consensus <- vctrs::vec_c(!!!lapply(seq_along(responses), function(idx) {
    extract_timestamp(responses[[idx]], receipt_list[[idx]])
  }))
  transaction_ids <- vapply(responses, extract_transaction_id, character(1), USE.NAMES = FALSE)
  acknowledged <- !is.na(receipt_status) & grepl("^SUCCESS", receipt_status)

  tibble::tibble(
    topic_id = rep(as.character(topic_id), length(responses)),
    chunk_number = chunk_numbers,
    transaction_id = transaction_ids,
    status = status,
    receipt_status = receipt_status,
    acknowledged = acknowledged,
    sequence_number = sequence,
    consensus_timestamp = consensus,
    response = responses
  )
}
