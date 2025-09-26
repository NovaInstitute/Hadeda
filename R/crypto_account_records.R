#' Retrieve recent transaction records for an account
#'
#' Call the CryptoService `getAccountRecords` RPC to fetch recent transaction
#' records associated with an account. The helper normalises the result into a
#' tibble with one row per transaction record.
#'
#' @inheritParams crypto_account_balance
#'
#' @return A tibble of transaction records.
#'
#' @export
crypto_account_records <- function(config,
                                   account_id,
                                   .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_account_records() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when querying account records.")
  }

  response <- hadeda_grpc_crypto_account_records(
    config = config,
    account_id = account_id
  )

  hadeda_parse_grpc_account_records(response)
}

#' @keywords internal
hadeda_grpc_crypto_account_records <- function(config, account_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_account_records %||% grpc$getAccountRecords %||%
    grpc$crypto_get_account_records %||% grpc$account_records
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService account records handler configured.\nProvide `config$grpc$get_account_records` to enable record queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id
  )
}
