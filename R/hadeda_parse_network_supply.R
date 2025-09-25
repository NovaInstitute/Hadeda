#' Parse network supply metrics
#'
#' Convert network supply payloads from the Mirror Node API into a tidy tibble.
#'
#' @param payload A list returned by the network supply endpoint.
#'
#' @return A tibble with supply measurements and their consensus timestamp.
#'
#' @keywords internal
hadeda_parse_network_supply <- function(payload) {
  if (is.null(payload) || length(payload) == 0) {
    return(tibble::tibble(
      collected_timestamp = lubridate::as_datetime(numeric(), tz = "UTC"),
      released_supply = numeric(),
      total_supply = numeric(),
      circulating_supply = numeric(),
      max_supply = numeric()
    ))
  }

  timestamp <- payload$timestamp %||% NA_character_

  tibble::tibble(
    collected_timestamp = hadeda_parse_timestamp(c(timestamp)),
    released_supply = as.numeric(payload$released_supply %||% payload$releasedSupply %||% NA_real_),
    total_supply = as.numeric(payload$total_supply %||% payload$totalSupply %||% NA_real_),
    circulating_supply = as.numeric(payload$circulating_supply %||% payload$circulatingSupply %||% NA_real_),
    max_supply = as.numeric(payload$max_supply %||% payload$maxSupply %||% NA_real_)
  )
}

