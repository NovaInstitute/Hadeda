#' Approve crypto allowances for spenders
#'
#' Submit a CryptoService `approveAllowances` transaction. The helper accepts
#' tidy allowance definitions for hbar, fungible tokens, and NFTs, normalises
#' the payloads, and delegates to a user-supplied gRPC handler.
#'
#' @inheritParams crypto_update_account
#' @param hbar_allowances Optional hbar allowance definitions supplied as a data
#'   frame or list with `spender_account_id` and `amount` fields. An optional
#'   `owner_account_id` column can override the operator account.
#' @param token_allowances Optional token allowance definitions with
#'   `token_id`, `spender_account_id`, and `amount` fields. Include an optional
#'   `owner_account_id` column to override the operator account.
#' @param nft_allowances Optional NFT allowance definitions with `token_id` and
#'   `spender_account_id` fields plus either `serial_numbers` or `all_serials`.
#'
#' @return A tibble summarising the acknowledgement returned by the handler.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' allowances <- tibble::tibble(
#'   spender_account_id = "0.0.5678",
#'   amount = 1000
#' )
#' \dontrun{
#'   crypto_approve_allowances(mirror, hbar_allowances = allowances)
#' }
#'
#' @export
crypto_approve_allowances <- function(config,
                                      hbar_allowances = NULL,
                                      token_allowances = NULL,
                                      nft_allowances = NULL,
                                      wait_for_receipt = TRUE,
                                      .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_approve_allowances() currently supports only the gRPC transport.")
  }

  hbar <- hadeda_normalise_hbar_allowances(hbar_allowances)
  token <- hadeda_normalise_token_allowances(token_allowances, require_amount = TRUE)
  nft <- hadeda_normalise_nft_allowances(nft_allowances)

  if (length(hbar) + length(token) + length(nft) == 0) {
    cli::cli_abort("Provide at least one allowance to approve.")
  }

  response <- hadeda_grpc_crypto_approve_allowances(
    config = config,
    hbar_allowances = hbar,
    token_allowances = token,
    nft_allowances = nft,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      hbar_allowances = list(hbar),
      token_allowances = list(token),
      nft_allowances = list(nft)
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_approve_allowances <- function(config,
                                                  hbar_allowances = list(),
                                                  token_allowances = list(),
                                                  nft_allowances = list(),
                                                  wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$approve_allowances %||% grpc$crypto_approve_allowances
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService allowance approval handler configured.\nProvide `config$grpc$approve_allowances` to enable allowance approvals.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    hbar_allowances = hbar_allowances,
    token_allowances = token_allowances,
    nft_allowances = nft_allowances,
    wait_for_receipt = wait_for_receipt
  )
}

#' Revoke crypto allowances
#'
#' Submit a CryptoService `deleteAllowances` transaction to remove token or NFT
#' allowances. The helper normalises tidy allowance definitions and delegates to
#' a configured gRPC handler.
#'
#' @inheritParams crypto_approve_allowances
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' deletions <- tibble::tibble(
#'   token_id = "0.0.6001",
#'   spender_account_id = "0.0.5678"
#' )
#' \dontrun{
#'   crypto_delete_allowances(mirror, token_allowances = deletions)
#' }
#'
#' @export
crypto_delete_allowances <- function(config,
                                     token_allowances = NULL,
                                     nft_allowances = NULL,
                                     wait_for_receipt = TRUE,
                                     .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_delete_allowances() currently supports only the gRPC transport.")
  }

  token <- hadeda_normalise_token_allowances(token_allowances, require_amount = FALSE)
  nft <- hadeda_normalise_nft_allowances(nft_allowances)

  if (length(token) + length(nft) == 0) {
    cli::cli_abort("Provide at least one allowance to delete.")
  }

  response <- hadeda_grpc_crypto_delete_allowances(
    config = config,
    token_allowances = token,
    nft_allowances = nft,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      token_allowances = list(token),
      nft_allowances = list(nft)
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_delete_allowances <- function(config,
                                                 token_allowances = list(),
                                                 nft_allowances = list(),
                                                 wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$delete_allowances %||% grpc$crypto_delete_allowances
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService allowance deletion handler configured.\nProvide `config$grpc$delete_allowances` to enable allowance deletions.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    token_allowances = token_allowances,
    nft_allowances = nft_allowances,
    wait_for_receipt = wait_for_receipt
  )
}
