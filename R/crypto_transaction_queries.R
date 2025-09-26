#' Retrieve transaction receipts via the CryptoService
#'
#' Call the `getTransactionReceipts` RPC to collect receipts for a given
#' transaction identifier. The helper returns a tibble summarising each receipt
#' entry and preserves the raw payload for downstream inspection.
#'
#' @inheritParams transactions_get
#' @param transaction_id Transaction identifier string.
#'
#' @return A tibble with receipt metadata.
#'
#' @export
crypto_transaction_receipts <- function(config,
                                        transaction_id,
                                        .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_transaction_receipts() currently supports only the gRPC transport.")
  }

  if (missing(transaction_id) || is.null(transaction_id) || identical(transaction_id, "")) {
    cli::cli_abort("`transaction_id` is required when requesting transaction receipts.")
  }

  response <- hadeda_grpc_crypto_transaction_receipts(
    config = config,
    transaction_id = transaction_id
  )

  hadeda_parse_grpc_transaction_receipts(response)
}

#' @keywords internal
hadeda_grpc_crypto_transaction_receipts <- function(config, transaction_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_transaction_receipts %||% grpc$getTransactionReceipts %||%
    grpc$crypto_get_transaction_receipts %||% grpc$transaction_receipts
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService transaction receipt handler configured.\nProvide `config$grpc$get_transaction_receipts` to enable receipt queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    transaction_id = transaction_id
  )
}

#' Retrieve a single transaction record
#'
#' Invoke the CryptoService `getTxRecordByTxID` query to fetch the full
#' transaction record for a transaction identifier.
#'
#' @inheritParams crypto_transaction_receipts
#' @param include_duplicates Logical toggle requesting duplicate transaction
#'   records for the identifier.
#' @param include_child_records Logical toggle requesting child transaction
#'   records spawned by the identifier.
#'
#' @return A tibble with a single transaction record.
#'
#' @export
crypto_transaction_record <- function(config,
                                      transaction_id,
                                      include_duplicates = FALSE,
                                      include_child_records = FALSE,
                                      .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_transaction_record() currently supports only the gRPC transport.")
  }

  if (missing(transaction_id) || is.null(transaction_id) || identical(transaction_id, "")) {
    cli::cli_abort("`transaction_id` is required when requesting a transaction record.")
  }

  response <- hadeda_grpc_crypto_transaction_record(
    config = config,
    transaction_id = transaction_id,
    include_duplicates = include_duplicates,
    include_child_records = include_child_records
  )

  hadeda_parse_grpc_transaction_record_response(response)
}

#' @keywords internal
hadeda_grpc_crypto_transaction_record <- function(config,
                                                  transaction_id,
                                                  include_duplicates = FALSE,
                                                  include_child_records = FALSE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_tx_record_by_tx_id %||% grpc$getTxRecordByTxID %||%
    grpc$get_transaction_record %||% grpc$getTransactionRecord %||%
    grpc$crypto_get_transaction_record %||% grpc$transaction_record
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService transaction record handler configured.\nProvide `config$grpc$get_tx_record_by_tx_id` to enable record queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    transaction_id = transaction_id,
    include_duplicates = include_duplicates,
    include_child_records = include_child_records
  )
}

#' Retrieve transaction records for a transaction ID
#'
#' Request duplicate or child records for a transaction identifier. The helper
#' returns one row per duplicate or child record along with the parsed receipt
#' and transfer metadata.
#' 
#' @inheritParams crypto_transaction_receipts
#' @param include_duplicates Logical toggle requesting duplicate transaction
#'   records for the identifier. Defaults to `TRUE` for convenience.
#' @param include_child_records Logical toggle requesting child transaction
#'   records spawned by the identifier. Defaults to `TRUE`.
#' 
#' @return A tibble with one row per record.
#' 
#' @export
crypto_transaction_records <- function(config,
                                       transaction_id,
                                       include_duplicates = TRUE,
                                       include_child_records = TRUE,
                                       .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_transaction_records() currently supports only the gRPC transport.")
  }

  if (missing(transaction_id) || is.null(transaction_id) || identical(transaction_id, "")) {
    cli::cli_abort("`transaction_id` is required when requesting transaction records.")
  }

  response <- hadeda_grpc_crypto_transaction_record(
    config = config,
    transaction_id = transaction_id,
    include_duplicates = include_duplicates,
    include_child_records = include_child_records
  )

  hadeda_parse_grpc_transaction_record_sets(
    response,
    include_duplicates = include_duplicates,
    include_child_records = include_child_records
  )
}
