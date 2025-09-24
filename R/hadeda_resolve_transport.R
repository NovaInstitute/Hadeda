#' Determine the default transport for a configuration
#'
#' Decide which transport should be used when none is specified explicitly.
#'
#' @param default_transport Optional character string naming the desired default transport.
#' @param rest_config The resolved configuration list for the REST transport.
#' @param grpc_config The resolved configuration list for the gRPC transport.
#'
#' @return A scalar character string of either "rest" or "grpc".
#'
#' @examples
#' defaults <- hadeda_network_defaults("testnet")
#' hadeda_resolve_transport(NULL, defaults$rest, defaults$grpc)
#'
#' hadeda_resolve_transport("grpc", defaults$rest, defaults$grpc)
#'
#' @export
hadeda_resolve_transport <- function(default_transport = NULL,
                                     rest_config = NULL,
                                     grpc_config = NULL) {
  transports <- c("rest", "grpc")

  rest_config <- rest_config %||% list()
  grpc_config <- grpc_config %||% list()

  if (is.null(default_transport)) {
    has_rest <- length(rest_config) > 0
    has_grpc <- length(grpc_config) > 0
    if (has_rest && !has_grpc) {
      return("rest")
    }
    if (!has_rest && has_grpc) {
      return("grpc")
    }
    return("rest")
  }

  choice <- tolower(default_transport[[1]])
  if (!choice %in% transports) {
    cli::cli_abort(
      "`default_transport` must be one of {transports}.",
      class = "hadeda_invalid_transport"
    )
  }
  choice
}
