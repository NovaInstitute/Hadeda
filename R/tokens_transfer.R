#' Transfer Hedera tokens via gRPC
#'
#' Submit a TokenService `transferTokens` transaction using the configured
#' gRPC transport. The helper accepts tidy token transfer specifications and
#' returns a normalised acknowledgement tibble.
#'
#' @param config Configuration created by [hadeda_config()].
#' @param token_transfers Data frame or list describing token transfers. Must
#'   include `token_id`, `account_id`, and `amount` fields. Optional
#'   `serial_numbers` entries are forwarded to the handler for NFT transfers.
#' @param memo Optional transaction memo forwarded to the gRPC handler.
#' @param max_fee Optional fee ceiling expressed in tinybars.
#' @param wait_for_receipt Logical indicating whether the gRPC handler should
#'   wait for a transaction receipt acknowledgement.
#' @param .transport Optional transport override. Only the gRPC transport is
#'   supported.
#'
#' @return A tibble containing acknowledgement metadata for the transfer
#'   transaction.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' transfers <- tibble::tibble(
#'   token_id = "0.0.6001",
#'   account_id = "0.0.2002",
#'   amount = 100
#' )
#' \dontrun{
#'   tokens_transfer(mirror, transfers)
#' }
#'
#' @export
tokens_transfer <- function(config,
                             token_transfers,
                             memo = NULL,
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
    cli::cli_abort("tokens_transfer() currently supports only the gRPC transport.")
  }

  transfers <- hadeda_normalise_token_transfers(token_transfers)
  if (!length(transfers)) {
    cli::cli_abort("`token_transfers` must contain at least one transfer definition.")
  }

  response <- hadeda_grpc_tokens_transfer(
    config = config,
    token_transfers = transfers,
    memo = memo,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(token_transfers = list(transfers))
  )
}

#' @keywords internal
hadeda_grpc_tokens_transfer <- function(config,
                                        token_transfers,
                                        memo = NULL,
                                        max_fee = NULL,
                                        wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$token_transfer %||% grpc$transfer_tokens %||% grpc$tokens_transfer
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC TokenService transfer handler configured.\nProvide `config$grpc$token_transfer` to enable token transfers.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    token_transfers = token_transfers,
    memo = memo,
    max_fee = max_fee,
    wait_for_receipt = wait_for_receipt
  )
}
