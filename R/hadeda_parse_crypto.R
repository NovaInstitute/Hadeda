#' Parse gRPC mutation responses for CryptoService helpers
#'
#' Normalises mutation acknowledgements returned by gRPC handlers into a tidy
#' tibble with consistent columns.
#'
#' @param response A list returned by a gRPC handler.
#' @param extra Named list of additional columns to append to the tibble.
#'
#' @return A tibble with transaction metadata.
#'
#' @keywords internal
hadeda_parse_grpc_mutation_response <- function(response, extra = list()) {
  if (is.null(response)) {
    response <- list()
  }
  receipt <- response$receipt %||% response$transaction_receipt %||% response$transactionReceipt
  if (is.null(receipt)) {
    receipt <- list()
  }

  transaction_id <- response$transaction_id %||% response$transactionId %||% response$transactionID
  status <- response$status %||% response$precheck_code %||% response$node_transaction_precheck_code %||% response$nodeTransactionPrecheckCode
  receipt_status <- receipt$status %||% receipt$receipt_status %||% receipt$receiptStatus

  consensus_ts <- response$consensus_timestamp %||% response$consensusTimestamp %||% receipt$consensus_timestamp %||% receipt$consensusTimestamp
  if (is.null(consensus_ts)) {
    consensus <- lubridate::as_datetime(NA_real_)
  } else {
    consensus <- hadeda_parse_timestamp(as.character(consensus_ts))
  }

  result <- tibble::tibble(
    transaction_id = if (is.null(transaction_id)) NA_character_ else as.character(transaction_id),
    status = if (is.null(status)) NA_character_ else as.character(status),
    receipt_status = if (is.null(receipt_status)) NA_character_ else as.character(receipt_status),
    consensus_timestamp = consensus,
    receipt = list(receipt),
    response = list(response)
  )

  if (length(extra)) {
    for (name in names(extra)) {
      result[[name]] <- extra[[name]]
    }
  }

  result
}

#' Normalise hbar transfer definitions
#'
#' Convert transfer specifications supplied as data frames or lists into a
#' canonical list form for downstream gRPC handlers.
#'
#' @param transfers Transfer specification. Either a data frame with
#'   `account_id` and `amount` columns or a list of such mappings.
#'
#' @return A list of transfer mappings.
#'
#' @keywords internal
hadeda_normalise_hbar_transfers <- function(transfers) {
  if (rlang::is_missing(transfers) || is.null(transfers)) {
    cli::cli_abort("`transfers` must be supplied when submitting a crypto transfer.")
  }

  if (tibble::is_tibble(transfers) || is.data.frame(transfers)) {
    if (!all(c("account_id", "amount") %in% names(transfers))) {
      cli::cli_abort("`transfers` data frames must include `account_id` and `amount` columns.")
    }
    transfers <- lapply(seq_len(nrow(transfers)), function(idx) {
      list(
        account_id = as.character(transfers$account_id[[idx]]),
        amount = transfers$amount[[idx]]
      )
    })
  } else if (is.list(transfers)) {
    transfers <- lapply(transfers, function(xfer) {
      if (!is.list(xfer) || is.null(xfer$account_id) || is.null(xfer$amount)) {
        cli::cli_abort("Each transfer must provide `account_id` and `amount` fields.")
      }
      list(
        account_id = as.character(xfer$account_id[[1]]),
        amount = xfer$amount[[1]]
      )
    })
  } else {
    cli::cli_abort("`transfers` must be a data frame or list of transfer definitions.")
  }

  transfers
}

#' Normalise token transfer definitions
#'
#' @param token_transfers Optional token transfer specification supplied as a
#'   data frame or list of mappings with `token_id`, `account_id`, and `amount`
#'   fields.
#'
#' @return A list of token transfer mappings.
#'
#' @keywords internal
hadeda_normalise_token_transfers <- function(token_transfers) {
  if (rlang::is_missing(token_transfers) || is.null(token_transfers)) {
    return(list())
  }

  if (tibble::is_tibble(token_transfers) || is.data.frame(token_transfers)) {
    required <- c("token_id", "account_id", "amount")
    if (!all(required %in% names(token_transfers))) {
      cli::cli_abort("`token_transfers` must include token_id, account_id, and amount columns.")
    }
    return(lapply(seq_len(nrow(token_transfers)), function(idx) {
      row <- token_transfers[idx, , drop = FALSE]
      list(
        token_id = as.character(row$token_id[[1]]),
        account_id = as.character(row$account_id[[1]]),
        amount = row$amount[[1]],
        serial_numbers = row$serial_numbers[[1]] %||% row$serialNumbers[[1]] %||% NULL
      )
    }))
  }

  if (is.list(token_transfers)) {
    return(lapply(token_transfers, function(entry) {
      if (!is.list(entry) || is.null(entry$token_id) || is.null(entry$account_id) || is.null(entry$amount)) {
        cli::cli_abort("Each token transfer must supply token_id, account_id, and amount fields.")
      }
      list(
        token_id = as.character(entry$token_id[[1]]),
        account_id = as.character(entry$account_id[[1]]),
        amount = entry$amount[[1]],
        serial_numbers = entry$serial_numbers %||% entry$serialNumbers %||% NULL
      )
    }))
  }

  cli::cli_abort("`token_transfers` must be supplied as a data frame or list of mappings.")
}
