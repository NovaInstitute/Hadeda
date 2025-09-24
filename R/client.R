#' Build a Hadeda transport configuration
#'
#' Instead of relying on a mutable client object, Hadeda exposes a set of
#' pure helpers that derive configuration for both REST and gRPC
#' transports.  Callers can thread the resulting list through downstream
#' functions or pipelines, keeping the code idiomatic for functional R
#' workflows.
#'
#' @param network One of "mainnet", "testnet", "previewnet", or
#'   "localhost". The value seeds sensible defaults for both the REST
#'   and gRPC transports.
#' @param rest,grpc Optional lists overriding the default REST or gRPC
#'   configuration derived from `network`. Named elements replace the
#'   defaults on a shallow level.
#' @param default_transport Which transport to prefer when a verb does
#'   not explicitly request one. Either "rest" or "grpc"; defaults to
#'   the most available transport for `network`.
#'
#' @return A named list with entries `network`, `rest`, `grpc`, and
#'   `default_transport`.
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

#' Merge default and user-supplied transport configuration
#'
#' @param defaults A list providing the baseline configuration for a
#'   transport.
#' @param override An optional list of overrides supplied by the user.
#'
#' @return A list with the same structure as `defaults` but with
#'   elements from `override` replacing those at the top level.
#' @export
hadeda_merge_config <- function(defaults = NULL, override = NULL) {
  if (is.null(defaults) && is.null(override)) {
    return(list())
  }
  if (!is.null(defaults) && !is.list(defaults)) {
    cli::cli_abort("`defaults` must be a list or NULL")
  }
  if (!is.null(override) && !is.list(override)) {
    cli::cli_abort("`override` must be a list or NULL")
  }

  defaults <- defaults %||% list()
  override <- override %||% list()
  utils::modifyList(defaults, override, keep.null = TRUE)
}

#' Determine the default transport for a configuration
#'
#' @param default_transport Optional character string naming the desired
#'   default transport.
#' @param rest_config,grpc_config The resolved configuration lists for
#'   each transport.
#'
#' @return A scalar character string of either "rest" or "grpc".
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

hadeda_network_defaults <- function(network) {
  switch(
    network,
    mainnet = list(
      rest = list(
        base_url = "https://mainnet-public.mirrornode.hedera.com/api/v1",
        rate_limit = list(requests_per_second = 10)
      ),
      grpc = list(
        host = "mainnet-public.grpc.hedera.com",
        port = 50212,
        tls = TRUE
      )
    ),
    testnet = list(
      rest = list(
        base_url = "https://testnet.mirrornode.hedera.com/api/v1",
        rate_limit = list(requests_per_second = 10)
      ),
      grpc = list(
        host = "testnet.grpc.hedera.com",
        port = 50212,
        tls = TRUE
      )
    ),
    previewnet = list(
      rest = list(
        base_url = "https://previewnet.mirrornode.hedera.com/api/v1",
        rate_limit = list(requests_per_second = 10)
      ),
      grpc = list(
        host = "previewnet.grpc.hedera.com",
        port = 50212,
        tls = TRUE
      )
    ),
    localhost = list(
      rest = list(
        base_url = "http://localhost:5551/api/v1",
        rate_limit = list(requests_per_second = Inf)
      ),
      grpc = list(
        host = "127.0.0.1",
        port = 50211,
        tls = FALSE
      )
    ),
    cli::cli_abort("Unknown Hedera network: {network}")
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
