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
      serial_numbers <- NULL
      if ("serial_numbers" %in% names(row)) {
        serial_numbers <- row$serial_numbers[[1]]
      } else if ("serialNumbers" %in% names(row)) {
        serial_numbers <- row$serialNumbers[[1]]
      }

      list(
        token_id = as.character(row$token_id[[1]]),
        account_id = as.character(row$account_id[[1]]),
        amount = row$amount[[1]],
        serial_numbers = serial_numbers
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

#' @keywords internal
hadeda_normalise_hbar_allowances <- function(allowances) {
  if (rlang::is_missing(allowances) || is.null(allowances)) {
    return(list())
  }

  normalise <- function(entry) {
    if (!is.list(entry)) {
      cli::cli_abort("Each allowance must be supplied as a list or data frame row.")
    }

    owner <- entry$owner_account_id %||% entry$ownerAccountId %||% NULL
    spender <- entry$spender_account_id %||% entry$spenderAccountId
    amount <- entry$amount

    if (is.null(spender) || is.null(amount)) {
      cli::cli_abort("Hbar allowances require `spender_account_id` and `amount` fields.")
    }

    list(
      owner_account_id = if (is.null(owner)) NULL else as.character(owner),
      spender_account_id = as.character(spender),
      amount = amount
    )
  }

  if (tibble::is_tibble(allowances) || is.data.frame(allowances)) {
    required <- c("spender_account_id", "amount")
    if (!all(required %in% names(allowances))) {
      cli::cli_abort("Hbar allowance data frames must include spender_account_id and amount columns.")
    }
    return(lapply(seq_len(nrow(allowances)), function(idx) {
      normalise(as.list(allowances[idx, , drop = FALSE]))
    }))
  }

  if (is.list(allowances)) {
    return(lapply(allowances, normalise))
  }

  cli::cli_abort("Hbar allowances must be supplied as a data frame or list of mappings.")
}

#' @keywords internal
hadeda_normalise_token_allowances <- function(allowances, require_amount = TRUE) {
  if (rlang::is_missing(allowances) || is.null(allowances)) {
    return(list())
  }

  normalise <- function(entry) {
    if (!is.list(entry)) {
      cli::cli_abort("Each allowance must be supplied as a list or data frame row.")
    }

    owner <- entry$owner_account_id %||% entry$ownerAccountId %||% NULL
    spender <- entry$spender_account_id %||% entry$spenderAccountId
    token_id <- entry$token_id %||% entry$tokenId
    amount <- entry$amount

    if (is.null(spender) || is.null(token_id)) {
      cli::cli_abort("Token allowances require `token_id` and `spender_account_id` fields.")
    }
    if (require_amount && is.null(amount)) {
      cli::cli_abort("Token allowances require an `amount` when approving allowances.")
    }

    list(
      owner_account_id = if (is.null(owner)) NULL else as.character(owner),
      spender_account_id = as.character(spender),
      token_id = as.character(token_id),
      amount = if (is.null(amount)) NULL else amount
    )
  }

  if (tibble::is_tibble(allowances) || is.data.frame(allowances)) {
    required <- c("token_id", "spender_account_id")
    if (!all(required %in% names(allowances))) {
      cli::cli_abort("Token allowance data frames must include token_id and spender_account_id columns.")
    }
    if (require_amount && !"amount" %in% names(allowances)) {
      cli::cli_abort("Token allowance approvals require an `amount` column.")
    }
    return(lapply(seq_len(nrow(allowances)), function(idx) {
      normalise(as.list(allowances[idx, , drop = FALSE]))
    }))
  }

  if (is.list(allowances)) {
    return(lapply(allowances, normalise))
  }

  cli::cli_abort("Token allowances must be supplied as a data frame or list of mappings.")
}

#' @keywords internal
hadeda_normalise_nft_allowances <- function(allowances) {
  if (rlang::is_missing(allowances) || is.null(allowances)) {
    return(list())
  }

  normalise <- function(entry) {
    if (!is.list(entry)) {
      cli::cli_abort("Each allowance must be supplied as a list or data frame row.")
    }

    owner <- entry$owner_account_id %||% entry$ownerAccountId %||% NULL
    spender <- entry$spender_account_id %||% entry$spenderAccountId
    token_id <- entry$token_id %||% entry$tokenId
    serial_numbers <- entry$serial_numbers %||% entry$serialNumbers
    all_serials <- entry$all_serials %||% entry$allSerials

    if (is.null(spender) || is.null(token_id)) {
      cli::cli_abort("NFT allowances require `token_id` and `spender_account_id` fields.")
    }

    if (is.list(serial_numbers) && length(serial_numbers) == 1) {
      serial_numbers <- serial_numbers[[1]]
    }

    list(
      owner_account_id = if (is.null(owner)) NULL else as.character(owner),
      spender_account_id = as.character(spender),
      token_id = as.character(token_id),
      serial_numbers = if (is.null(serial_numbers)) NULL else serial_numbers,
      all_serials = if (is.null(all_serials)) NULL else rlang::is_true(all_serials)
    )
  }

  if (tibble::is_tibble(allowances) || is.data.frame(allowances)) {
    required <- c("token_id", "spender_account_id")
    if (!all(required %in% names(allowances))) {
      cli::cli_abort("NFT allowance data frames must include token_id and spender_account_id columns.")
    }
    return(lapply(seq_len(nrow(allowances)), function(idx) {
      normalise(as.list(allowances[idx, , drop = FALSE]))
    }))
  }

  if (is.list(allowances)) {
    return(lapply(allowances, normalise))
  }

  cli::cli_abort("NFT allowances must be supplied as a data frame or list of mappings.")
}

#' @keywords internal
hadeda_parse_grpc_account_balance <- function(response) {
  if (is.null(response)) {
    response <- list()
  }

  account <- response$account_id %||% response$accountId %||% response$accountID %||% NA_character_
  balance <- response$balance %||% response$hbar_balance %||% response$hbarBalance %||% NA_real_
  timestamp <- response$timestamp %||% response$consensus_timestamp %||% response$consensusTimestamp
  if (is.null(timestamp) && !is.null(response$header)) {
    timestamp <- response$header$timestamp %||% response$header$timestamp_seconds
  }

  token_balances <- response$token_balances %||% response$tokenBalances %||% list()

  tibble::tibble(
    account = as.character(account),
    balance = suppressWarnings(as.numeric(balance)),
    timestamp = hadeda_parse_timestamp(as.character(timestamp %||% NA_character_)),
    tokens = list(token_balances)
  )
}

#' @keywords internal
hadeda_parse_grpc_account_info <- function(response) {
  if (is.null(response)) {
    response <- list()
  }

  account <- response$account_id %||% response$accountId %||% response$accountID %||% NA_character_
  token_relationships <- response$token_relationships %||% response$tokenRelationships %||% list()

  tibble::tibble(
    account_id = as.character(account),
    alias = response$alias %||% NA_character_,
    memo = response$memo %||% NA_character_,
    key = list(response$key %||% list()),
    deleted = rlang::is_true(response$deleted),
    receiver_sig_required = rlang::is_true(response$receiver_sig_required %||% response$receiverSigRequired),
    balance = suppressWarnings(as.numeric(response$balance %||% response$cryptocurrency_balance %||% NA_real_)),
    token_relationships = list(token_relationships),
    response = list(response)
  )
}

#' @keywords internal
hadeda_parse_grpc_account_details <- function(response) {
  if (is.null(response)) {
    response <- list()
  }

  account <- response$account_id %||% response$accountId %||% response$accountID %||% NA_character_

  tibble::tibble(
    account_id = as.character(account),
    alias = response$alias %||% NA_character_,
    memo = response$memo %||% NA_character_,
    contract_account_id = response$contract_account_id %||% response$contractAccountId %||% NA_character_,
    key = list(response$key %||% list()),
    evm_address = response$evm_address %||% response$evmAddress %||% NA_character_,
    max_automatic_token_associations = suppressWarnings(as.integer(response$max_automatic_token_associations %||% response$maxAutomaticTokenAssociations %||% NA_integer_)),
    staking = list(response$staking_info %||% response$stakingInfo %||% list()),
    response = list(response)
  )
}

#' @keywords internal
hadeda_parse_grpc_account_records <- function(response) {
  records <- response$records %||% response$transaction_records %||% response$transactionRecords %||% list()
  hadeda_parse_grpc_transaction_records(list(records = records))
}

#' @keywords internal
hadeda_parse_grpc_transaction_record <- function(record) {
  if (is.null(record)) {
    record <- list()
  }

  transaction_id <- record$transaction_id %||% record$transactionId %||% record$transactionID %||% NA_character_
  consensus <- record$consensus_timestamp %||% record$consensusTimestamp %||% NA_character_

  tibble::tibble(
    transaction_id = as.character(transaction_id),
    consensus_timestamp = hadeda_parse_timestamp(as.character(consensus)),
    memo = record$memo %||% NA_character_,
    receipt = list(record$receipt %||% record$transaction_receipt %||% record$transactionReceipt %||% list()),
    transfers = list(record$transfers %||% record$transfer_list %||% record$transferList %||% list()),
    record = list(record)
  )
}

#' @keywords internal
hadeda_parse_grpc_transaction_records <- function(response) {
  records <- response$records %||% response$transaction_records %||% response$transactionRecords %||% list()
  if (length(records) == 0) {
    return(tibble::tibble(
      transaction_id = character(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      memo = character(),
      receipt = list(),
      transfers = list(),
      record = list()
    ))
  }

  vctrs::vec_rbind(!!!lapply(records, hadeda_parse_grpc_transaction_record))
}

#' @keywords internal
hadeda_parse_grpc_transaction_receipts <- function(response) {
  receipts <- response$receipts %||% response$transaction_receipts %||% response$transactionReceipts %||% list()
  if (length(receipts) == 0) {
    return(tibble::tibble(
      status = character(),
      account_id = character(),
      transaction_id = character(),
      receipt = list()
    ))
  }

  tibble::tibble(
    status = purrr::map_chr(receipts, ~as.character(purrr::pluck(.x, "status", .default = NA_character_))),
    account_id = purrr::map_chr(receipts, ~as.character(purrr::pluck(.x, "account_id", .default = purrr::pluck(.x, "accountId", .default = purrr::pluck(.x, "accountID", .default = NA_character_))))),
    transaction_id = purrr::map_chr(receipts, ~as.character(purrr::pluck(.x, "transaction_id", .default = purrr::pluck(.x, "transactionId", .default = purrr::pluck(.x, "transactionID", .default = NA_character_))))),
    receipt = receipts
  )
}
