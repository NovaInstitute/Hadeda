#' Default configuration for known Hedera networks
#'
#' Provide sensible transport defaults for named Hedera networks.
#'
#' @param network One of "mainnet", "testnet", "previewnet", or "localhost".
#'
#' @return A list containing default `rest` and `grpc` configurations.
#'
#' @examples
#' hadeda_network_defaults("testnet")
#'
#' @keywords internal
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
