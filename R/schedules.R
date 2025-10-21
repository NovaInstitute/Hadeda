#' List scheduled transactions
#'
#' Retrieve scheduled transactions from the Hedera Mirror Node REST API with
#' optional filters.
#'
#' @inheritParams transactions_list
#' @param schedule_id Optional schedule identifier filter.
#' @param transaction_id Optional scheduling transaction identifier filter.
#' @param scheduled_timestamp Optional scheduled execution timestamp filter.
#' @param executed_timestamp Optional executed timestamp filter.
#' @param deleted Optional logical flag to restrict deleted vs active schedules.
#'
#' @return A tibble describing scheduled transactions.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   schedules_list(mirror, limit = 5)
#' }
#'
#' @export
schedules_list <- function(config,
                           limit = NULL,
                           order = c("desc", "asc"),
                           schedule_id = NULL,
                           transaction_id = NULL,
                           scheduled_timestamp = NULL,
                           executed_timestamp = NULL,
                           deleted = NULL,
                           timestamp = NULL,
                           .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = TRUE,
    grpc_supported = FALSE
  )
  if (!identical(transport, "rest")) {
    cli::cli_abort("schedules_list() currently supports only the REST transport.")
  }

  query <- hadeda_drop_null(list(
    limit = limit,
    order = match.arg(order),
    `schedule.id` = schedule_id,
    transactionId = transaction_id,
    scheduledTimestamp = scheduled_timestamp,
    executedTimestamp = executed_timestamp,
    deleted = deleted,
    timestamp = timestamp
  ))

  responses <- hadeda_rest_paginate(config, "schedules", query)
  records <- purrr::map(responses, "schedules")
  records <- purrr::flatten(records)
  hadeda_parse_schedules(records)
}

#' Get schedule details
#'
#' Fetch a single scheduled transaction from the Mirror Node REST API.
#'
#' @inheritParams schedules_list
#'
#' @return A tibble with a single row describing the requested schedule.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   schedules_get(mirror, schedule_id = "0.0.8001")
#' }
#'
#' @export
schedules_get <- function(config,
                          schedule_id,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = TRUE,
    grpc_supported = FALSE
  )
  if (!identical(transport, "rest")) {
    cli::cli_abort("schedules_get() currently supports only the REST transport.")
  }

  if (missing(schedule_id) || is.null(schedule_id)) {
    cli::cli_abort("schedule_id is required to retrieve a schedule.")
  }

  resp <- hadeda_rest_get(config, paste0("schedules/", schedule_id))
  record <- resp[["schedule"]] %||% resp
  if (is.null(record) || length(record) == 0) {
    return(hadeda_parse_schedules(list()))
  }

  while (
    is.list(record) &&
      length(record) == 1 &&
      is.list(record[[1]]) &&
      !rlang::has_name(record, "schedule_id") &&
      !rlang::has_name(record, "scheduleId")
  ) {
    record <- record[[1]]
  }

  hadeda_parse_schedules(list(record))
}

#' Create a scheduled transaction via gRPC
#'
#' Submit a ScheduleService `createSchedule` transaction using the configured
#' gRPC transport.
#'
#' @inheritParams crypto_transfer
#' @param scheduled_transaction The transaction body to schedule, provided as a
#'   list or raw bytes understood by the configured handler.
#' @param payer_account_id Optional account responsible for transaction fees.
#' @param admin_key Optional key list granting administrative control over the
#'   schedule.
#' @param memo Optional human-readable memo.
#' @param wait_for_expiry Optional logical indicating whether execution should
#'   wait for expiration if signatures are incomplete.
#' @param expiration_time Optional expiration timestamp expressed as POSIXct,
#'   a numeric epoch value, or a string timestamp.
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_receipt Logical indicating whether the gRPC handler should
#'   wait for a transaction receipt acknowledgement.
#'
#' @return A tibble summarising the scheduled transaction acknowledgement.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   schedule_create(
#'     mirror,
#'     scheduled_transaction = list(type = "cryptoTransfer"),
#'     memo = "Demo schedule"
#'   )
#' }
#'
#' @export
schedule_create <- function(config,
                            scheduled_transaction,
                            payer_account_id = NULL,
                            admin_key = NULL,
                            memo = NULL,
                            wait_for_expiry = NULL,
                            expiration_time = NULL,
                            max_fee = NULL,
                            wait_for_receipt = TRUE,
                            .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("schedule_create() currently supports only the gRPC transport.")
  }

  if (missing(scheduled_transaction) || is.null(scheduled_transaction)) {
    cli::cli_abort("scheduled_transaction is required when creating a schedule.")
  }

  normalise_timestamp <- function(value) {
    if (rlang::is_missing(value) || is.null(value)) {
      return(list(value = NULL, column = lubridate::as_datetime(NA_real_)))
    }
    if (inherits(value, "POSIXt")) {
      return(list(value = value, column = value))
    }
    if (is.numeric(value)) {
      dt <- lubridate::as_datetime(value)
      return(list(value = value, column = dt))
    }
    if (is.character(value)) {
      parsed <- hadeda_parse_timestamp(value)
      return(list(value = value, column = parsed))
    }
    cli::cli_abort("`expiration_time` must be NULL, POSIXct, numeric seconds, or a timestamp string.")
  }

  expiry <- normalise_timestamp(expiration_time)

  response <- hadeda_grpc_schedule_create(
    config = config,
    scheduled_transaction = scheduled_transaction,
    payer_account_id = payer_account_id,
    admin_key = admin_key,
    memo = memo,
    wait_for_expiry = wait_for_expiry,
    expiration_time = expiry$value,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  receipt <- response$receipt %||% response$transaction_receipt %||% response$transactionReceipt %||% list()
  schedule_id <- receipt$schedule_id %||% receipt$scheduleId %||% receipt$scheduleID %||%
    response$schedule_id %||% response$scheduleId %||% response$scheduleID
  scheduled_id <- receipt$scheduled_transaction_id %||% receipt$scheduledTransactionId %||%
    response$scheduled_transaction_id %||% response$scheduledTransactionId

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      schedule_id = if (is.null(schedule_id)) NA_character_ else as.character(schedule_id),
      scheduled_transaction_id = if (is.null(scheduled_id)) NA_character_ else as.character(scheduled_id),
      payer_account_id = if (is.null(payer_account_id)) NA_character_ else as.character(payer_account_id),
      memo = if (is.null(memo)) NA_character_ else as.character(memo),
      wait_for_expiry = if (is.null(wait_for_expiry)) NA else as.logical(wait_for_expiry),
      scheduled_transaction = list(scheduled_transaction),
      admin_key = list(admin_key %||% list())
    )
  )
  tbl$expiration_time <- expiry$column
  tbl
}

#' Sign a scheduled transaction
#'
#' Add signatures to a scheduled transaction using the gRPC transport.
#'
#' @inheritParams schedule_create
#' @param schedule_id The schedule identifier to sign.
#' @param signatures Optional collection of signatures expected by the handler.
#'
#' @return A tibble summarising the acknowledgement.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   schedule_sign(mirror, schedule_id = "0.0.8001")
#' }
#'
#' @export
schedule_sign <- function(config,
                          schedule_id,
                          signatures = NULL,
                          max_fee = NULL,
                          wait_for_receipt = TRUE,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("schedule_sign() currently supports only the gRPC transport.")
  }

  if (missing(schedule_id) || is.null(schedule_id) || identical(schedule_id, "")) {
    cli::cli_abort("schedule_id is required when signing a schedule.")
  }

  response <- hadeda_grpc_schedule_sign(
    config = config,
    schedule_id = schedule_id,
    signatures = signatures,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      schedule_id = as.character(schedule_id),
      signatures = list(signatures %||% list())
    )
  )
}

#' Delete a scheduled transaction
#'
#' Submit a ScheduleService `deleteSchedule` transaction using the gRPC
#' transport.
#'
#' @inheritParams schedule_sign
#'
#' @return A tibble summarising the acknowledgement.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   schedule_delete(mirror, schedule_id = "0.0.8001")
#' }
#'
#' @export
schedule_delete <- function(config,
                            schedule_id,
                            max_fee = NULL,
                            wait_for_receipt = TRUE,
                            .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("schedule_delete() currently supports only the gRPC transport.")
  }

  if (missing(schedule_id) || is.null(schedule_id) || identical(schedule_id, "")) {
    cli::cli_abort("schedule_id is required when deleting a schedule.")
  }

  response <- hadeda_grpc_schedule_delete(
    config = config,
    schedule_id = schedule_id,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(schedule_id = as.character(schedule_id))
  )
}

#' Retrieve schedule metadata via gRPC
#'
#' Query the ScheduleService `getScheduleInfo` RPC to fetch schedule metadata.
#'
#' @inheritParams schedule_sign
#'
#' @return A tibble with schedule metadata and the raw response.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   schedule_info(mirror, schedule_id = "0.0.8001")
#' }
#'
#' @export
schedule_info <- function(config,
                          schedule_id,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("schedule_info() currently supports only the gRPC transport.")
  }

  if (missing(schedule_id) || is.null(schedule_id) || identical(schedule_id, "")) {
    cli::cli_abort("schedule_id is required when requesting schedule info.")
  }

  response <- hadeda_grpc_schedule_info(
    config = config,
    schedule_id = schedule_id
  )

  info <- response$schedule_info %||% response$scheduleInfo %||% response
  tbl <- hadeda_parse_schedules(list(info))
  tbl$response <- list(response)
  tbl
}

#' @keywords internal
hadeda_grpc_schedule_create <- function(config,
                                        scheduled_transaction,
                                        payer_account_id = NULL,
                                        admin_key = NULL,
                                        memo = NULL,
                                        wait_for_expiry = NULL,
                                        expiration_time = NULL,
                                        max_fee = NULL,
                                        wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$schedule_create %||% grpc$create_schedule %||% grpc$createSchedule
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC ScheduleService create handler configured.\nProvide `config$grpc$schedule_create` to enable schedule creation.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    scheduled_transaction = scheduled_transaction,
    payer_account_id = payer_account_id,
    admin_key = admin_key,
    memo = memo,
    wait_for_expiry = wait_for_expiry,
    expiration_time = expiration_time,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' @keywords internal
hadeda_grpc_schedule_sign <- function(config,
                                      schedule_id,
                                      signatures = NULL,
                                      max_fee = NULL,
                                      wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$schedule_sign %||% grpc$sign_schedule %||% grpc$signSchedule
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC ScheduleService sign handler configured.\nProvide `config$grpc$schedule_sign` to enable schedule signatures.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    schedule_id = schedule_id,
    signatures = signatures,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' @keywords internal
hadeda_grpc_schedule_delete <- function(config,
                                        schedule_id,
                                        max_fee = NULL,
                                        wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$schedule_delete %||% grpc$delete_schedule %||% grpc$deleteSchedule
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC ScheduleService delete handler configured.\nProvide `config$grpc$schedule_delete` to enable schedule deletion.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    schedule_id = schedule_id,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' @keywords internal
hadeda_grpc_schedule_info <- function(config, schedule_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$schedule_info %||% grpc$get_schedule_info %||% grpc$getScheduleInfo
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC ScheduleService info handler configured.\nProvide `config$grpc$schedule_info` to enable schedule info queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    schedule_id = schedule_id
  )
}
