#' Update keys for an existing Hedera account
#'
#' Submit a CryptoService account update transaction that replaces the signing
#' key for an account. Only the gRPC transport is supported at this stage.
#'
#' @inheritParams crypto_create_account
#' @param account_id Identifier for the account to update.
#' @param public_key Optional replacement public key.
#' @param key_list Optional structured key list definition understood by the
#'   gRPC handler. Either `public_key` or `key_list` must be supplied.
#' @param wait_for_receipt Logical toggling whether the gRPC handler should wait
#'   for a receipt acknowledgement.
#'
#' @return A tibble summarising the transaction metadata.
#'
#' @export
crypto_update_account_keys <- function(config,
                                       account_id,
                                       public_key = NULL,
                                       key_list = NULL,
                                       memo = NULL,
                                       wait_for_receipt = TRUE,
                                       .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_update_account_keys() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when updating account keys.")
  }
  if ((is.null(public_key) || rlang::is_missing(public_key)) && (is.null(key_list) || rlang::is_missing(key_list))) {
    cli::cli_abort("Supply either `public_key` or `key_list` when updating account keys.")
  }

  response <- hadeda_grpc_crypto_update_account_keys(
    config = config,
    account_id = account_id,
    public_key = public_key,
    key_list = key_list,
    memo = memo,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      account_id = as.character(account_id),
      new_key = list(list(public_key = public_key, key_list = key_list))
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_update_account_keys <- function(config,
                                                   account_id,
                                                   public_key = NULL,
                                                   key_list = NULL,
                                                   memo = NULL,
                                                   wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$update_account_keys %||% grpc$crypto_update_account_keys %||% grpc$account_update
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService account update handler configured.\nProvide `config$grpc$update_account_keys` to enable key updates.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id,
    public_key = public_key,
    key_list = key_list,
    memo = memo,
    wait_for_receipt = wait_for_receipt
  )
}
