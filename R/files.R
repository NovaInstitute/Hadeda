#' Create a new file on Hedera
#'
#' Submit a FileService `createFile` transaction. Callers can provide file
#' contents as a raw vector, UTF-8 character string, numeric vector of byte
#' values, or a list of raw blocks. Optional key lists, memos, and expiration
#' timestamps are forwarded to the configured gRPC handler.
#'
#' @inheritParams crypto_account_balance
#' @param contents File contents supplied as raw bytes. Character vectors are
#'   automatically encoded as UTF-8.
#' @param keys Optional list describing the file keys accepted by the gRPC
#'   handler. Data frames are coerced into row-wise lists.
#' @param memo Optional memo attached to the transaction.
#' @param expiration_time Optional expiration timestamp expressed as POSIXct,
#'   a string timestamp, or seconds since the epoch.
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_receipt Logical indicating whether the gRPC handler should
#'   wait for a transaction receipt acknowledgement.
#' @param .transport Optional transport override. Only the gRPC transport is
#'   supported.
#'
#' @return A tibble summarising the transaction acknowledgement and resulting
#'   file identifier.
#'
#' @export
file_create <- function(config,
                        contents,
                        keys = NULL,
                        memo = NULL,
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
    cli::cli_abort("file_create() currently supports only the gRPC transport.")
  }

  payload <- hadeda_normalise_file_contents(contents, allow_null = FALSE, require_non_empty = TRUE)
  key_list <- hadeda_normalise_file_keys(keys)
  expiry <- hadeda_normalise_file_expiration(expiration_time)

  response <- hadeda_grpc_file_create(
    config = config,
    contents = payload,
    keys = key_list,
    memo = memo,
    expiration_time = expiry$value,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  receipt <- response$receipt %||% response$transaction_receipt %||% response$transactionReceipt %||% list()
  file_id <- receipt$file_id %||% receipt$fileId %||% receipt$fileID %||%
    response$file_id %||% response$fileId %||% response$fileID

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      file_id = if (is.null(file_id)) NA_character_ else as.character(file_id),
      contents = list(payload),
      keys = list(key_list %||% list()),
      memo = if (is.null(memo)) NA_character_ else as.character(memo)
    )
  )
  tbl$expiration_time <- expiry$column
  tbl
}

#' @keywords internal
hadeda_grpc_file_create <- function(config,
                                    contents,
                                    keys = NULL,
                                    memo = NULL,
                                    expiration_time = NULL,
                                    max_fee = NULL,
                                    wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$file_create %||% grpc$create_file %||% grpc$createFile
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC FileService create handler configured.\nProvide `config$grpc$file_create` to enable file creation.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    contents = contents,
    keys = keys,
    memo = memo,
    expiration_time = expiration_time,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' Update file metadata or contents
#'
#' Submit a FileService `updateFile` transaction. The helper accepts optional
#' content replacements, key lists, memos, and expiration timestamps. At least
#' one attribute must be provided.
#'
#' @inheritParams file_create
#' @param file_id File identifier to update.
#'
#' @export
file_update <- function(config,
                        file_id,
                        contents = NULL,
                        keys = NULL,
                        memo = NULL,
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
    cli::cli_abort("file_update() currently supports only the gRPC transport.")
  }

  if (missing(file_id) || is.null(file_id) || identical(file_id, "")) {
    cli::cli_abort("`file_id` is required when updating a file.")
  }

  payload <- hadeda_normalise_file_contents(contents, allow_null = TRUE)
  key_list <- hadeda_normalise_file_keys(keys)
  expiry <- hadeda_normalise_file_expiration(expiration_time)

  if (is.null(payload) && is.null(key_list) && is.null(memo) && is.null(expiry$value)) {
    cli::cli_abort("Provide at least one of contents, keys, memo, or expiration_time when updating a file.")
  }

  response <- hadeda_grpc_file_update(
    config = config,
    file_id = file_id,
    contents = payload,
    keys = key_list,
    memo = memo,
    expiration_time = expiry$value,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  tbl <- hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      file_id = as.character(file_id),
      contents = list(payload),
      keys = list(key_list %||% list()),
      memo = if (is.null(memo)) NA_character_ else as.character(memo)
    )
  )
  tbl$expiration_time <- expiry$column
  tbl
}

#' @keywords internal
hadeda_grpc_file_update <- function(config,
                                    file_id,
                                    contents = NULL,
                                    keys = NULL,
                                    memo = NULL,
                                    expiration_time = NULL,
                                    max_fee = NULL,
                                    wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$file_update %||% grpc$update_file %||% grpc$updateFile
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC FileService update handler configured.\nProvide `config$grpc$file_update` to enable file updates.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    file_id = file_id,
    contents = contents,
    keys = keys,
    memo = memo,
    expiration_time = expiration_time,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' Append content to a Hedera file
#'
#' Submit a FileService `appendContent` transaction that appends bytes to an
#' existing file. Contents must be provided as raw bytes or encodable character
#' data.
#'
#' @inheritParams file_update
#'
#' @export
file_append <- function(config,
                        file_id,
                        contents,
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
    cli::cli_abort("file_append() currently supports only the gRPC transport.")
  }

  if (missing(file_id) || is.null(file_id) || identical(file_id, "")) {
    cli::cli_abort("`file_id` is required when appending to a file.")
  }

  payload <- hadeda_normalise_file_contents(contents, allow_null = FALSE, require_non_empty = TRUE)

  response <- hadeda_grpc_file_append(
    config = config,
    file_id = file_id,
    contents = payload,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      file_id = as.character(file_id),
      contents = list(payload)
    )
  )
}

#' @keywords internal
hadeda_grpc_file_append <- function(config,
                                    file_id,
                                    contents,
                                    max_fee = NULL,
                                    wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$file_append %||% grpc$append_file %||% grpc$appendContent %||%
    grpc$append_content %||% grpc$fileAppend
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC FileService append handler configured.\nProvide `config$grpc$file_append` to enable file appends.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    file_id = file_id,
    contents = contents,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' Delete a Hedera file
#'
#' Submit a FileService `deleteFile` transaction to mark a file as deleted.
#'
#' @inheritParams file_append
#'
#' @export
file_delete <- function(config,
                        file_id,
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
    cli::cli_abort("file_delete() currently supports only the gRPC transport.")
  }

  if (missing(file_id) || is.null(file_id) || identical(file_id, "")) {
    cli::cli_abort("`file_id` is required when deleting a file.")
  }

  response <- hadeda_grpc_file_delete(
    config = config,
    file_id = file_id,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(file_id = as.character(file_id))
  )
}

#' @keywords internal
hadeda_grpc_file_delete <- function(config,
                                    file_id,
                                    max_fee = NULL,
                                    wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$file_delete %||% grpc$delete_file %||% grpc$deleteFile
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC FileService delete handler configured.\nProvide `config$grpc$file_delete` to enable file deletion.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    file_id = file_id,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}

#' Download file contents
#'
#' Invoke the FileService `getFileContent` RPC to retrieve file bytes. The
#' helper returns a tibble with the raw contents for downstream processing.
#'
#' @inheritParams file_delete
#'
#' @return A tibble with the file identifier, raw contents, and byte length.
#'
#' @export
file_content <- function(config,
                         file_id,
                         .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("file_content() currently supports only the gRPC transport.")
  }

  if (missing(file_id) || is.null(file_id) || identical(file_id, "")) {
    cli::cli_abort("`file_id` is required when requesting file contents.")
  }

  response <- hadeda_grpc_file_content(
    config = config,
    file_id = file_id
  )

  hadeda_parse_file_content(response)
}

#' @keywords internal
hadeda_grpc_file_content <- function(config, file_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$file_content %||% grpc$get_file_content %||% grpc$getFileContent %||%
    grpc$get_file_contents %||% grpc$getFileContents
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC FileService content handler configured.\nProvide `config$grpc$file_content` to enable file content queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    file_id = file_id
  )
}

#' Retrieve file metadata
#'
#' Invoke the FileService `getFileInfo` RPC to fetch file metadata including
#' size, memo, deletion status, keys, and expiration timestamp.
#'
#' @inheritParams file_delete
#'
#' @return A tibble describing the file metadata.
#'
#' @export
file_info <- function(config,
                      file_id,
                      .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("file_info() currently supports only the gRPC transport.")
  }

  if (missing(file_id) || is.null(file_id) || identical(file_id, "")) {
    cli::cli_abort("`file_id` is required when requesting file info.")
  }

  response <- hadeda_grpc_file_info(
    config = config,
    file_id = file_id
  )

  hadeda_parse_file_info(response)
}

#' @keywords internal
hadeda_grpc_file_info <- function(config, file_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$file_info %||% grpc$get_file_info %||% grpc$getFileInfo
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC FileService info handler configured.\nProvide `config$grpc$file_info` to enable file info queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    file_id = file_id
  )
}

#' @keywords internal
hadeda_normalise_file_contents <- function(contents,
                                           allow_null = FALSE,
                                           require_non_empty = FALSE) {
  if (rlang::is_missing(contents) || is.null(contents)) {
    if (allow_null) {
      return(NULL)
    }
    cli::cli_abort("`contents` must be supplied when interacting with Hedera files.")
  }

  normalised <- NULL
  if (is.raw(contents)) {
    normalised <- contents
  } else if (is.character(contents)) {
    normalised <- charToRaw(paste0(contents, collapse = ""))
  } else if (is.list(contents) && all(purrr::map_lgl(contents, is.raw))) {
    normalised <- do.call(c, contents)
  } else if (is.numeric(contents) || is.integer(contents)) {
    values <- as.integer(contents)
    if (any(values < 0 | values > 255, na.rm = TRUE)) {
      cli::cli_abort("Numeric contents must contain values between 0 and 255.")
    }
    normalised <- as.raw(values)
  } else {
    cli::cli_abort("`contents` must be a raw vector, character vector, numeric byte vector, or list of raw vectors.")
  }

  if (require_non_empty && length(normalised) == 0) {
    cli::cli_abort("`contents` must not be empty for this operation.")
  }

  normalised
}

#' @keywords internal
hadeda_normalise_file_keys <- function(keys) {
  if (rlang::is_missing(keys) || is.null(keys)) {
    return(NULL)
  }

  if (tibble::is_tibble(keys) || is.data.frame(keys)) {
    return(lapply(seq_len(nrow(keys)), function(idx) {
      as.list(keys[idx, , drop = FALSE])
    }))
  }

  if (!is.list(keys)) {
    cli::cli_abort("`keys` must be supplied as a list or data frame of key definitions.")
  }

  keys
}

#' @keywords internal
hadeda_normalise_file_expiration <- function(expiration_time) {
  if (rlang::is_missing(expiration_time) || is.null(expiration_time)) {
    return(list(value = NULL, column = lubridate::as_datetime(NA_real_)))
  }

  if (inherits(expiration_time, "POSIXt")) {
    return(list(value = expiration_time, column = expiration_time))
  }

  if (is.numeric(expiration_time)) {
    dt <- lubridate::as_datetime(expiration_time)
    return(list(value = expiration_time, column = dt))
  }

  if (is.character(expiration_time)) {
    parsed <- hadeda_parse_timestamp(expiration_time)
    return(list(value = expiration_time, column = parsed))
  }

  cli::cli_abort("`expiration_time` must be NULL, POSIXct, numeric seconds, or a timestamp string.")
}

#' @keywords internal
hadeda_parse_file_content <- function(response) {
  if (is.null(response)) {
    response <- list()
  }

  file_id <- response$file_id %||% response$fileId %||% response$fileID
  contents <- response$contents %||% response$file_contents %||% response$fileContents %||% raw()
  payload <- hadeda_normalise_file_contents(contents, allow_null = TRUE)
  if (is.null(payload)) {
    payload <- raw()
  }

  tibble::tibble(
    file_id = if (is.null(file_id)) NA_character_ else as.character(file_id),
    contents = list(payload),
    size = length(payload),
    response = list(response)
  )
}

#' @keywords internal
hadeda_parse_file_info <- function(response) {
  if (is.null(response)) {
    response <- list()
  }

  info <- response$file_info %||% response$fileInfo %||% response
  file_id <- info$file_id %||% info$fileId %||% info$fileID
  expiration <- info$expiration_time %||% info$expirationTime
  if (is.list(expiration) && !inherits(expiration, "POSIXt")) {
    seconds <- expiration$seconds %||% expiration$secondsSinceEpoch %||% 0
    nanos <- expiration$nanos %||% expiration$nano %||% 0
    expiration <- as.numeric(seconds) + (as.numeric(nanos) / 1e9)
  }
  expiry <- hadeda_normalise_file_expiration(expiration)

  tibble::tibble(
    file_id = if (is.null(file_id)) NA_character_ else as.character(file_id),
    size = suppressWarnings(as.integer(info$size %||% info$file_size %||% info$fileSize %||% NA_integer_)),
    memo = info$memo %||% info$file_memo %||% info$fileMemo %||% NA_character_,
    deleted = rlang::is_true(info$deleted %||% info$is_deleted %||% info$isDeleted),
    keys = list(info$keys %||% info$key_list %||% info$keyList %||% list()),
    ledger_id = info$ledger_id %||% info$ledgerId %||% NA_character_,
    expiration_time = expiry$column,
    response = list(response)
  )
}
