#' Delete a Hedera account
#'
#' Submit a CryptoService account delete transaction that transfers any
#' remaining hbar to a designated beneficiary. Only the gRPC transport is
#' currently supported.
#'
#' @inheritParams crypto_update_account
#' @param transfer_account_id Account that should receive the deleted account's
#'   remaining hbar balance.
#'
#' @return A tibble summarising the transaction metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   crypto_delete(
#'     mirror,
#'     account_id = "0.0.1234",
#'     transfer_account_id = "0.0.5678"
#'   )
#' }
#'
#' @export
crypto_delete <- function(config,
                          account_id,
                          transfer_account_id,
                          wait_for_receipt = TRUE,
                          .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_delete() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when deleting an account.")
  }
  if (missing(transfer_account_id) || is.null(transfer_account_id) || identical(transfer_account_id, "")) {
    cli::cli_abort("`transfer_account_id` is required when deleting an account.")
  }

  response <- hadeda_grpc_crypto_delete(
    config = config,
    account_id = account_id,
    transfer_account_id = transfer_account_id,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      account_id = as.character(account_id),
      transfer_account_id = as.character(transfer_account_id)
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_delete <- function(config,
                                      account_id,
                                      transfer_account_id,
                                      wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$delete_account %||% grpc$crypto_delete
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService delete handler configured.\nProvide `config$grpc$delete_account` to enable account deletion.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id,
    transfer_account_id = transfer_account_id,
    wait_for_receipt = wait_for_receipt
  )
}
