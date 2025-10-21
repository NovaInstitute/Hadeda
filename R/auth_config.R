#' Configure Hadeda with operator credentials
#'
#' This helper wraps [hadeda_config()] and injects operator credentials into the
#' gRPC transport. It validates the supplied account identifier and private key
#' (defaulting to `HADEDA_OPERATOR_ID` / `HADEDA_OPERATOR_KEY`) and, when
#' possible, prepares a signing function using [hadeda_grpc_ed25519_signer()].
#'
#' @param network Hedera network to target (`"testnet"`, `"mainnet"`,
#'   `"previewnet"`, or `"localhost"`). Defaults to `"testnet"`.
#' @param operator_id Operator account identifier. Defaults to
#'   `Sys.getenv("HADEDA_OPERATOR_ID")`.
#' @param operator_key Operator private key (PEM). Defaults to
#'   `Sys.getenv("HADEDA_OPERATOR_KEY")`.
#' @param rest Optional overrides for the REST configuration (passed to
#'   [hadeda_config()]).
#' @param grpc Optional overrides for the gRPC configuration. Values supplied
#'   here take precedence over the defaults injected by this helper, so you can
#'   provide channel factories or pre-built handlers.
#' @param default_transport Desired default transport. Defaults to `"grpc"`,
#'   ensuring that gRPC helpers are available without further changes.
#' @param signer Optional signing function. When `NULL`, the helper attempts to
#'   construct one from `operator_key` using [hadeda_grpc_ed25519_signer()]. Set
#'   this to `FALSE` to skip automatic signer creation.
#'
#' @return A configuration list compatible with `hadeda` helpers.
#' @export
hadeda_auth_config <- function(
    network = c("testnet", "mainnet", "previewnet", "localhost"),
    operator_id = Sys.getenv("HADEDA_OPERATOR_ID"),
    operator_key = Sys.getenv("HADEDA_OPERATOR_KEY"),
    rest = NULL,
    grpc = NULL,
    default_transport = "grpc",
    signer = NULL) {

  network <- rlang::arg_match(network)
  creds <- hadeda_grpc_env_credentials(
    account_id = operator_id,
    private_key = operator_key
  )

  grpc <- hadeda_merge_config(list(), grpc)
  grpc$operator_account_id <- creds$operator_account_id
  grpc$operator_private_key <- creds$operator_private_key

  if (!identical(signer, FALSE) && is.null(grpc$sign_transaction)) {
    if (is.null(signer)) {
      if (requireNamespace("openssl", quietly = TRUE)) {
        signer <- hadeda_grpc_ed25519_signer(creds$operator_private_key)
      }
    }
    if (!is.null(signer)) {
      grpc$sign_transaction <- signer
    }
  }

  hadeda_config(
    network = network,
    rest = rest,
    grpc = grpc,
    default_transport = default_transport %||% "grpc"
  )
}
