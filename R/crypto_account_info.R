#' Retrieve account metadata via the CryptoService
#'
#' Query the Hedera `getAccountInfo` RPC and return a tidy tibble with summary
#' metadata alongside the raw response for downstream inspection.
#'
#' @inheritParams crypto_account_balance
#'
#' @return A tibble with one row containing account metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   crypto_account_info(mirror, account_id = "0.0.1234")
#' }
#'
#' @export
crypto_account_info <- function(config,
                                account_id,
                                .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_account_info() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when requesting account info.")
  }

  response <- hadeda_grpc_crypto_account_info(
    config = config,
    account_id = account_id
  )

  hadeda_parse_grpc_account_info(response)
}

#' @keywords internal
hadeda_grpc_crypto_account_info <- function(config, account_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_account_info %||% grpc$getAccountInfo %||%
    grpc$crypto_get_account_info %||% grpc$account_info
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService account info handler configured.\nProvide `config$grpc$get_account_info` to enable info queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id
  )
}

#' Retrieve rich account metadata (HIP-623)
#'
#' Invoke the `getAccountDetails` RPC to return HIP-623 account metadata. The
#' helper preserves the nested staking and key structures in list-columns for
#' downstream inspection.
#'
#' @inheritParams crypto_account_balance
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   crypto_account_details(mirror, account_id = "0.0.1234")
#' }
#'
#' @export
crypto_account_details <- function(config,
                                   account_id,
                                   .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_account_details() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when requesting account details.")
  }

  response <- hadeda_grpc_crypto_account_details(
    config = config,
    account_id = account_id
  )

  hadeda_parse_grpc_account_details(response)
}

#' @keywords internal
hadeda_grpc_crypto_account_details <- function(config, account_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_account_details %||% grpc$getAccountDetails %||%
    grpc$crypto_get_account_details
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService account details handler configured.\nProvide `config$grpc$get_account_details` to enable details queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id
  )
}
