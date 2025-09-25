#' Parse network node metadata
#'
#' Transform node metadata payloads into a tidy tibble.
#'
#' @param records A list of node objects from the Mirror Node network endpoints.
#'
#' @return A tibble describing network nodes.
#'
#' @keywords internal
hadeda_parse_network_nodes <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble(
      node_id = integer(),
      account_id = character(),
      description = character(),
      service_endpoints = list(),
      stake = numeric(),
      min_stake = numeric(),
      max_stake = numeric()
    ))
  }

  tibble::tibble(
    node_id = purrr::map_int(records, function(record) {
      value <- record$node_id %||% record$nodeId %||% NA_integer_
      as.integer(value)
    }),
    account_id = purrr::map_chr(records, function(record) {
      record$account_id %||% record$accountId %||% NA_character_
    }),
    description = purrr::map_chr(records, function(record) {
      record$description %||% NA_character_
    }),
    service_endpoints = purrr::map(records, function(record) {
      endpoints <- record$service_endpoints %||% record$serviceEndpoints %||% list()
      purrr::map(endpoints, function(endpoint) {
        list(
          ip_address_v4 = endpoint$ip_address_v4 %||% endpoint$ipAddressV4 %||% NA_character_,
          port = endpoint$port %||% NA_integer_
        )
      })
    }),
    stake = purrr::map_dbl(records, function(record) {
      value <- record$stake %||% NA_real_
      as.numeric(value)
    }),
    min_stake = purrr::map_dbl(records, function(record) {
      value <- record$min_stake %||% record$minStake %||% NA_real_
      as.numeric(value)
    }),
    max_stake = purrr::map_dbl(records, function(record) {
      value <- record$max_stake %||% record$maxStake %||% NA_real_
      as.numeric(value)
    })
  )
}
