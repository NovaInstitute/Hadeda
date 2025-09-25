#' Parse Hedera schedules
#'
#' Convert scheduled transaction records from Mirror Node REST payloads or
#' ScheduleService gRPC responses into tidy tibbles.
#'
#' @param records A list of schedule records.
#'
#' @return A tibble describing scheduled transactions.
#'
#' @keywords internal
hadeda_parse_schedules <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      schedule_id = character(),
      transaction_id = character(),
      scheduled_transaction_id = character(),
      creator_account_id = character(),
      payer_account_id = character(),
      consensus_timestamp = lubridate::as_datetime(numeric()),
      executed_timestamp = lubridate::as_datetime(numeric()),
      expiration_time = lubridate::as_datetime(numeric()),
      deletion_timestamp = lubridate::as_datetime(numeric()),
      wait_for_expiry = logical(),
      memo = character(),
      admin_key = list(),
      signatures = list(),
      transaction_body = list(),
      ledger_id = character(),
      deleted = logical()
    ))
  }

  make_timestamp <- function(value) {
    if (inherits(value, "POSIXt")) {
      return(format(as.numeric(value), digits = 15, scientific = FALSE, trim = TRUE))
    }
    if (is.numeric(value)) {
      return(format(value, digits = 15, scientific = FALSE, trim = TRUE))
    }
    if (is.character(value)) {
      return(value)
    }
    if (is.list(value)) {
      seconds <- value$seconds %||% value$seconds_since_epoch %||% value$secondsSinceEpoch %||% value$seconds %||% NA_real_
      nanos <- value$nanos %||% value$nano %||% value$fractional_seconds %||% 0
      sec_num <- suppressWarnings(as.numeric(seconds))
      nano_num <- suppressWarnings(as.numeric(nanos))
      if (is.na(sec_num)) {
        return(NA_character_)
      }
      if (is.na(nano_num)) {
        nano_num <- 0
      }
      total <- sec_num + (nano_num / 1e9)
      return(format(total, digits = 15, scientific = FALSE, trim = TRUE))
    }
    NA_character_
  }

  parse_timestamp <- function(values) {
    hadeda_parse_timestamp(vapply(values, make_timestamp, character(1), USE.NAMES = FALSE))
  }

  safe_chr <- function(x) {
    if (is.null(x)) {
      NA_character_
    } else {
      as.character(x)
    }
  }

  safe_logical <- function(x) {
    if (is.null(x)) {
      NA
    } else {
      as.logical(x)
    }
  }

  collect_signatures <- function(record) {
    sigs <- record$signatures %||% record$signature_map %||% record$signatureMap
    if (!is.null(sigs)) {
      return(hadeda_parse_schedule_signatures(sigs))
    }
    signers <- record$signers %||% record$signature_list %||% record$signatureList
    if (!is.null(signers)) {
      return(signers)
    }
    list()
  }

  tibble::tibble(
    schedule_id = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "schedule_id",
        .default = purrr::pluck(.x, "scheduleId", .default = purrr::pluck(.x, "scheduleID", .default = NULL))
      )
    )),
    transaction_id = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "transaction_id",
        .default = purrr::pluck(
          .x,
          "transactionId",
          .default = purrr::pluck(.x, "transactionID", .default = NULL)
        )
      )
    )),
    scheduled_transaction_id = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "scheduled_transaction_id",
        .default = purrr::pluck(
          .x,
          "scheduledTransactionId",
          .default = purrr::pluck(.x, "scheduledTransactionID", .default = NULL)
        )
      )
    )),
    creator_account_id = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "creator_account_id",
        .default = purrr::pluck(
          .x,
          "creatorAccountId",
          .default = purrr::pluck(.x, "creator_account", .default = purrr::pluck(.x, "creatorAccount", .default = NULL))
        )
      )
    )),
    payer_account_id = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "payer_account_id",
        .default = purrr::pluck(
          .x,
          "payerAccountId",
          .default = purrr::pluck(.x, "payer_account", .default = purrr::pluck(.x, "payerAccount", .default = NULL))
        )
      )
    )),
    consensus_timestamp = parse_timestamp(purrr::map(records, ~ purrr::pluck(
      .x,
      "consensus_timestamp",
      .default = purrr::pluck(.x, "consensusTimestamp", .default = NULL)
    ))),
    executed_timestamp = parse_timestamp(purrr::map(records, ~ purrr::pluck(
      .x,
      "executed_timestamp",
      .default = purrr::pluck(
        .x,
        "execution_time",
        .default = purrr::pluck(.x, "executionTime", .default = NULL)
      )
    ))),
    expiration_time = parse_timestamp(purrr::map(records, ~ purrr::pluck(
      .x,
      "expiration_time",
      .default = purrr::pluck(.x, "expirationTime", .default = NULL)
    ))),
    deletion_timestamp = parse_timestamp(purrr::map(records, ~ purrr::pluck(
      .x,
      "deletion_timestamp",
      .default = purrr::pluck(.x, "deletionTimestamp", .default = NULL)
    ))),
    wait_for_expiry = purrr::map_lgl(records, ~ safe_logical(
      purrr::pluck(
        .x,
        "wait_for_expiry",
        .default = purrr::pluck(
          .x,
          "waitForExpiry",
          .default = purrr::pluck(.x, "wait_for_expiration", .default = purrr::pluck(.x, "waitForExpiration", .default = NULL))
        )
      )
    )),
    memo = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "memo",
        .default = purrr::pluck(
          .x,
          "schedule_memo",
          .default = purrr::pluck(.x, "scheduleMemo", .default = NULL)
        )
      )
    )),
    admin_key = purrr::map(records, ~ purrr::pluck(
      .x,
      "admin_key",
      .default = purrr::pluck(.x, "adminKey", .default = NULL)
    )),
    signatures = purrr::map(records, collect_signatures),
    transaction_body = purrr::map(records, ~ purrr::pluck(
      .x,
      "transaction_body",
      .default = purrr::pluck(
        .x,
        "scheduled_transaction_body",
        .default = purrr::pluck(.x, "scheduledTransactionBody", .default = NULL)
      )
    )),
    ledger_id = purrr::map_chr(records, ~ safe_chr(
      purrr::pluck(
        .x,
        "ledger_id",
        .default = purrr::pluck(.x, "ledgerId", .default = NULL)
      )
    )),
    deleted = purrr::map_lgl(records, ~ safe_logical(
      purrr::pluck(
        .x,
        "deleted",
        .default = purrr::pluck(
          .x,
          "is_deleted",
          .default = purrr::pluck(.x, "isDeleted", .default = NULL)
        )
      )
    ))
  )
}

#' Parse schedule signatures
#'
#' Convert schedule signature payloads into a tibble.
#'
#' @param signatures A list of schedule signature entries.
#'
#' @return A tibble of signatures.
#'
#' @keywords internal
hadeda_parse_schedule_signatures <- function(signatures) {
  if (is.null(signatures) || length(signatures) == 0) {
    return(tibble::tibble(
      public_key_prefix = character(),
      signature = character(),
      type = character(),
      status = character()
    ))
  }

  if (!rlang::is_list(signatures)) {
    signatures <- list(signatures)
  }

  tibble::tibble(
    public_key_prefix = purrr::map_chr(signatures, ~ {
      value <- purrr::pluck(
        .x,
        "public_key_prefix",
        .default = purrr::pluck(.x, "publicKeyPrefix", .default = purrr::pluck(.x, "pubKeyPrefix", .default = NULL))
      )
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    signature = purrr::map_chr(signatures, ~ {
      value <- purrr::pluck(.x, "signature", .default = purrr::pluck(.x, "signature_bytes", .default = purrr::pluck(.x, "signatureBytes", .default = NULL)))
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    type = purrr::map_chr(signatures, ~ {
      value <- purrr::pluck(
        .x,
        "type",
        .default = purrr::pluck(.x, "signature_type", .default = purrr::pluck(.x, "signatureType", .default = NULL))
      )
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    }),
    status = purrr::map_chr(signatures, ~ {
      value <- purrr::pluck(
        .x,
        "status",
        .default = purrr::pluck(.x, "verification_status", .default = purrr::pluck(.x, "verificationStatus", .default = NULL))
      )
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    })
  )
}
