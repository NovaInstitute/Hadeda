#' Resolve a transport for a Hadeda helper
#'
#' Determine which transport to use for a helper call, respecting configuration and overrides.
#'
#' @param config A configuration list from [hadeda_config()].
#' @param .transport Optional override for the transport, either "rest" or "grpc".
#' @param rest_supported Logical indicating whether the helper supports REST.
#' @param grpc_supported Logical indicating whether the helper supports gRPC.
#' @param caller Environment used for error messaging.
#'
#' @return A character scalar naming the chosen transport.
#'
#' @examples
#' config <- hadeda_config()
#' hadeda_choose_transport(config, .transport = NULL)
#' hadeda_choose_transport(config, .transport = "rest")
#'
#' @keywords internal
hadeda_choose_transport <- function(config, .transport = NULL,
                                     rest_supported = TRUE,
                                     grpc_supported = TRUE,
                                     caller = rlang::caller_env()) {
  transports <- c("rest", "grpc")

  choice <- .transport %||% config$default_transport %||%
    if (rest_supported) "rest" else "grpc"

  if (!choice %in% transports) {
    cli::cli_abort(
      "Unknown transport '{choice}'.",
      class = "hadeda_invalid_transport",
      call = caller
    )
  }

  if (choice == "rest" && !rest_supported) {
    cli::cli_abort(
      "This helper requires the gRPC transport; the REST transport is not supported.",
      class = "hadeda_unsupported_transport",
      call = caller
    )
  }
  if (choice == "grpc" && !grpc_supported) {
    cli::cli_abort(
      "This helper requires the REST transport; the gRPC transport is not supported.",
      class = "hadeda_unsupported_transport",
      call = caller
    )
  }

  choice
}
