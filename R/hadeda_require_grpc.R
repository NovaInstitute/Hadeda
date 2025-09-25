#' Ensure a gRPC configuration is present
#'
#' Validate that a configuration includes gRPC settings required for RPC calls.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param caller Environment used for error messaging.
#'
#' @return The gRPC configuration list.
#'
#' @examples
#' config <- hadeda_config()
#' hadeda_require_grpc(config)
#'
#' @keywords internal
hadeda_require_grpc <- function(config, caller = rlang::caller_env()) {
  grpc <- config$grpc
  if (is.null(grpc) || !is.list(grpc) || is.null(grpc$host) || is.null(grpc$port)) {
    cli::cli_abort(
      "gRPC configuration requires both `host` and `port`.\nDid you create the configuration with `hadeda_config()`?",
      class = "hadeda_missing_grpc_config",
      call = caller
    )
  }
  grpc$port <- as.integer(grpc$port)
  grpc
}
