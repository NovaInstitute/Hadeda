#' Update mutable properties for an existing Hedera account
#'
#' Submit a CryptoService account update transaction. The helper delegates to a
#' user-supplied gRPC handler and normalises the acknowledgement into a tidy
#' tibble. Only the gRPC transport is currently supported.
#'
#' @inheritParams crypto_update_account_keys
#' @param auto_renew_period Optional auto renew period in seconds.
#' @param max_token_associations Optional ceiling for automatic token
#'   associations.
#' @param staked_node_id Optional node identifier to stake to.
#' @param staked_account_id Optional account identifier to stake to.
#' @param decline_reward Optional logical controlling staking reward decline.
#' @param receiver_sig_required Optional logical toggling
#'   `receiver_sig_required`.
#' @param wait_for_receipt Logical toggling whether the handler should await a
#'   transaction receipt acknowledgement.
#'
#' @return A tibble summarising the transaction metadata.
#'
#' @examples
#' mirror <- hadeda_config(network = "testnet")
#' \dontrun{
#'   crypto_update_account(
#'     mirror,
#'     account_id = "0.0.1234",
#'     memo = "Updated account memo"
#'   )
#' }
#'
#' @export
crypto_update_account <- function(config,
                                  account_id,
                                  memo = NULL,
                                  auto_renew_period = NULL,
                                  max_token_associations = NULL,
                                  staked_node_id = NULL,
                                  staked_account_id = NULL,
                                  decline_reward = NULL,
                                  receiver_sig_required = NULL,
                                  wait_for_receipt = TRUE,
                                  .transport = NULL) {
  transport <- hadeda_choose_transport(
    config,
    .transport,
    rest_supported = FALSE,
    grpc_supported = TRUE
  )
  if (!identical(transport, "grpc")) {
    cli::cli_abort("crypto_update_account() currently supports only the gRPC transport.")
  }

  if (missing(account_id) || is.null(account_id) || identical(account_id, "")) {
    cli::cli_abort("`account_id` is required when updating an account.")
  }

  response <- hadeda_grpc_crypto_update_account(
    config = config,
    account_id = account_id,
    memo = memo,
    auto_renew_period = auto_renew_period,
    max_token_associations = max_token_associations,
    staked_node_id = staked_node_id,
    staked_account_id = staked_account_id,
    decline_reward = decline_reward,
    receiver_sig_required = receiver_sig_required,
    wait_for_receipt = wait_for_receipt
  )

  hadeda_parse_grpc_mutation_response(
    response,
    extra = list(
      account_id = as.character(account_id),
      memo = memo %||% NA_character_,
      auto_renew_period = if (is.null(auto_renew_period)) NA_real_ else as.numeric(auto_renew_period),
      max_token_associations = if (is.null(max_token_associations)) NA_real_ else as.numeric(max_token_associations),
      staked_node_id = staked_node_id %||% NA_character_,
      staked_account_id = staked_account_id %||% NA_character_,
      decline_reward = if (is.null(decline_reward)) NA else rlang::is_true(decline_reward),
      receiver_sig_required = if (is.null(receiver_sig_required)) NA else rlang::is_true(receiver_sig_required)
    )
  )
}

#' @keywords internal
hadeda_grpc_crypto_update_account <- function(config,
                                              account_id,
                                              memo = NULL,
                                              auto_renew_period = NULL,
                                              max_token_associations = NULL,
                                              staked_node_id = NULL,
                                              staked_account_id = NULL,
                                              decline_reward = NULL,
                                              receiver_sig_required = NULL,
                                              wait_for_receipt = TRUE) {
  grpc <- hadeda_require_grpc(config)
  handler <- grpc$update_account %||% grpc$crypto_update_account %||% grpc$account_update
  if (!rlang::is_function(handler)) {
    cli::cli_abort(
      "No gRPC CryptoService account update handler configured.\nProvide `config$grpc$update_account` to enable account updates.",
      class = "hadeda_grpc_not_configured"
    )
  }

  handler(
    config = config,
    account_id = account_id,
    memo = memo,
    auto_renew_period = auto_renew_period,
    max_token_associations = max_token_associations,
    staked_node_id = staked_node_id,
    staked_account_id = staked_account_id,
    decline_reward = decline_reward,
    receiver_sig_required = receiver_sig_required,
    wait_for_receipt = wait_for_receipt
  )
}
