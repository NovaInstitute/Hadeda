#' Query an account balance via the CryptoService
#'
#' Retrieve the latest hbar and token balances for an account using the gRPC
#' CryptoService `getAccountBalance` RPC. The helper delegates to a configured
#' handler and returns a tidy tibble matching the schema produced by
#' [accounts_balance()].
#'
#' @inheritParams accounts_balance
#'
#' @return A tibble with account, balance, timestamp, and token holdings.
#'
#' @export
crypto_account_balance <- function(config,
                                   account_id,
                                   .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_account_balance() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when querying an account balance.")
  }

  response <- hadeda_grpc_crypto_account_balance(
    config = config,
    account_id = account_id
  )

  hadeda_parse_grpc_account_balance(response)
}

#' @keywords internal
hadeda_grpc_crypto_account_balance <- function(config, account_id) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$get_account_balance %||% grpc$crypto_get_account_balance %||% grpc$account_balance
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService balance handler configured.\nProvide `config$grpc$get_account_balance` to enable balance queries.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id
  )
}
