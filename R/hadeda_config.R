#' Build a Hadeda transport configuration
#'
#' Construct transport settings for communicating with Hedera services.
#'
#' @param network One of "mainnet", "testnet", "previewnet", or "localhost".
#'   The value seeds sensible defaults for both the REST and gRPC transports.
#' @param rest Optional list overriding the default REST configuration.
#' @param grpc Optional list overriding the default gRPC configuration.
#' @param default_transport Which transport to prefer when not explicitly specified.
#'
#' @return A named list with entries `network`, `rest`, `grpc`, and `default_transport`.
#'
#' @examples
#' config <- hadeda_config(network = "testnet")
#' config$rest$base_url
#'
#' custom_config <- hadeda_config(
#'   network = "testnet",
#'   rest = list(rate_limit = list(requests_per_second = 5)),
#'   default_transport = "rest"
#' )
#' custom_config$rest$rate_limit
#'
#' @export
hadeda_config <- function(
    network = c("testnet", "mainnet", "previewnet", "localhost"),
    rest = NULL,
    grpc = NULL,
    default_transport = NULL) {
  network <- rlang::arg_match(network)
  defaults <- hadeda_network_defaults(network)

  rest_config <- hadeda_merge_config(defaults$rest, rest)
  grpc_config <- hadeda_merge_config(defaults$grpc, grpc)
  transport <- hadeda_resolve_transport(default_transport, rest_config, grpc_config)

  list(
    network = network,
    rest = rest_config,
    grpc = grpc_config,
    default_transport = transport
  )
}
