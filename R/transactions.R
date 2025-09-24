#' List Hedera transactions
#'
#' @param config A configuration list from [hadeda_config()].
#' @param limit Optional page size.
#' @param order Ordering of results.
#' @param transaction_type Optional transaction type filter.
#' @param account_id Optional account ID filter.
#' @param timestamp Optional timestamp range filter.
#' @param .transport Optional transport override.
#'
#' @return A tibble of transactions.
#' @export
transactions_list <- function(config,
                              limit = NULL,
                              order = c("desc", "asc"),
                              transaction_type = NULL,
                              account_id = NULL,
                              timestamp = NULL,
                              .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("transactions_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    transactiontype = transaction_type,
    account.id = account_id,
    timestamp = timestamp
  ))

  responses <- hadeda_rest_paginate(config, "transactions", query)
  records <- purrr::map(responses, "transactions")
  records <- purrr::flatten(records)
  hadeda_parse_transactions(records)
}

#' Retrieve a single Hedera transaction
#'
#' @inheritParams transactions_list
#' @param transaction_id The transaction identifier.
#'
#' @return A tibble with a single row describing the transaction.
#' @export
transactions_get <- function(config, transaction_id, .transport = NULL) {
  transport <- hadeda_choose_transport(config, .transport, rest_supported = TRUE, grpc_supported = FALSE)
  if (!identical(transport, "rest")) {
    cli::cli_abort("transactions_get() currently supports only the REST transport.")
  }

  resp <- hadeda_rest_get(config, paste0("transactions/", transaction_id))
  hadeda_parse_transactions(list(resp))
}

#' Retrieve messages for a topic
#'
#' @inheritParams transactions_list
#' @param topic_id Consensus topic identifier.
#'
#' @return A tibble of topic messages.
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

hadeda_parse_transactions <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      transaction_id = character(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      name = character(),
      valid_start = lubridate::as_datetime(numeric()),
      charged_tx_fee = numeric()
    ))
  }

  tibble::tibble(
    transaction_id = purrr::map_chr(records, "transaction_id", .default = NA_character_),
    consensus_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, "consensus_timestamp", .default = NA_character_)),
    name = purrr::map_chr(records, "name", .default = NA_character_),
    valid_start = hadeda_parse_timestamp(purrr::map_chr(records, "valid_start_timestamp", .default = NA_character_)),
    charged_tx_fee = purrr::map_dbl(records, "charged_tx_fee", .default = NA_real_)
  )
}

hadeda_parse_topic_messages <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      topic_id = character(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      message = character(),
      running_hash = character(),
      sequence_number = numeric()
    ))
  }

  tibble::tibble(
    topic_id = purrr::map_chr(records, "topic_id", .default = NA_character_),
    consensus_timestamp = hadeda_parse_timestamp(purrr::map_chr(records, "consensus_timestamp", .default = NA_character_)),
    message = purrr::map_chr(records, "message", .default = NA_character_),
    running_hash = purrr::map_chr(records, "running_hash", .default = NA_character_),
    sequence_number = purrr::map_dbl(records, "sequence_number", .default = NA_real_)
  )
}
