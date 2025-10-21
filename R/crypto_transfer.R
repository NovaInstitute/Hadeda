#' Submit a Hedera crypto transfer
#'
#' Issue a CryptoService transfer transaction using the gRPC transport. The
#' helper accepts tidy transfer specifications for hbar and token movements and
#' returns a tidy acknowledgement tibble.
#'
#' @param config Configuration list created by [hadeda_config()].
#' @param transfers Hbar transfer definitions supplied as a data frame or list
#'   with `account_id` and `amount` entries.
#' @param token_transfers Optional token transfer definitions supplied as a data
#'   frame or list with `token_id`, `account_id`, and `amount` fields.
#' @param memo Optional memo string for the transaction.
#' @param transaction_valid_duration Optional duration in seconds for which the
#'   transaction remains valid.
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_receipt Logical flag forwarded to the gRPC handler to control
#'   whether a receipt acknowledgement is required.
#' @param .transport Optional transport override. Only "grpc" is supported.
#'
#' @return A tibble summarising the resulting transaction metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' transfers <- tibble::tibble(
#'   account_id = c("0.0.1234", "0.0.5678"),
#'   amount = c(100, -100)
#' )
#' \dontrun{
#'   crypto_transfer(mirror, transfers = transfers)
#' }
#'
#' @export
crypto_transfer <- function(config,
                            transfers,
                            token_transfers = NULL,
                            memo = NULL,
                            transaction_valid_duration = NULL,
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
    cli::cli_abort("crypto_transfer() currently supports only the gRPC transport.")
  }

  normalised_transfers <- hadeda_normalise_hbar_transfers(transfers)
  token_transfer_list <- hadeda_normalise_token_transfers(token_transfers)

  response <- hadeda_grpc_crypto_transfer(
    config = config,
    transfers = normalised_transfers,
    token_transfers = token_transfer_list,
    memo = memo,
    transaction_valid_duration = transaction_valid_duration,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      transfers = list(normalised_transfers),
      token_transfers = list(token_transfer_list)
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_transfer <- function(config,
                                        transfers,
                                        token_transfers = list(),
                                        memo = NULL,
                                        transaction_valid_duration = NULL,
                                        max_fee = NULL,
                                        wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$transfer %||% grpc$crypto_transfer
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService transfer handler configured.\nProvide `config$grpc$transfer` to enable crypto transfers.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    transfers = transfers,
    token_transfers = token_transfers,
    memo = memo,
    transaction_valid_duration = transaction_valid_duration,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}
