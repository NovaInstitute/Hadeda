#' Deprecated live hash RPC stubs
#'
#' The Hedera network deprecated the CryptoService live hash RPCs. Hadeda
#' exposes placeholder helpers that throw a `NOT_SUPPORTED` error to preserve
#' surface parity with the historical SDKs.
#'
#' @inheritParams crypto_account_balance
#' @param hash Optional raw hash payload.
#'
#' @return These helpers always error.
#'
#' @keywords internal
crypto_livehash_add <- function(config, account_id, hash = NULL, .transport = NULL) {
  hadeda_abort_livehash()
}

#' @keywords internal
crypto_livehash_delete <- function(config, account_id, hash = NULL, .transport = NULL) {
  hadeda_abort_livehash()
}

#' @keywords internal
crypto_livehash_get <- function(config, account_id, .transport = NULL) {
  hadeda_abort_livehash()
}

#' @keywords internal
hadeda_abort_livehash <- function() {
  cli::cli_abort(
    "CryptoService live hash RPCs are deprecated and not supported.",
    class = "hadeda_not_supported"
  )
}
