#' Parse network exchange rate payloads
#'
#' Convert exchange rate payloads from the Mirror Node API into a tidy tibble.
#'
#' @param rates A list of rate records paired with their type labels.
#' @param effective_timestamp Optional effective timestamp shared by the payload.
#'
#' @return A tibble describing exchange rates for the Hedera network.
#'
#' @keywords internal
hadeda_parse_network_exchange_rate <- function(rates, effective_timestamp = NULL) {
  if (length(rates) == 0) {
    return(tibble::tibble(
      rate_type = character(),
      hbar_equivalent = numeric(),
      cent_equivalent = numeric(),
      exchange_rate = numeric(),
      expiration_time_seconds = numeric(),
      expiration_timestamp = lubridate::as_datetime(numeric(), tz = "UTC"),
      effective_timestamp = lubridate::as_datetime(numeric(), tz = "UTC")
    ))
  }

  rate_type <- purrr::map_chr(rates, function(rate) {
    rate$type %||% NA_character_
  })

  hbar_equivalent <- purrr::map_dbl(rates, function(rate) {
    value <- rate$rate$hbar_equivalent %||% rate$rate$hbarEquivalent %||% NA_real_
    as.numeric(value)
  })

  cent_equivalent <- purrr::map_dbl(rates, function(rate) {
    value <- rate$rate$cent_equivalent %||% rate$rate$centEquivalent %||% NA_real_
    as.numeric(value)
  })

  expiration_seconds <- purrr::map_dbl(rates, function(rate) {
    value <- rate$rate$expiration_time %||% rate$rate$expirationTime %||% NA_real_
    as.numeric(value)
  })

  if (!is.null(effective_timestamp) && length(effective_timestamp) > 0) {
    effective_raw <- rep(as.character(effective_timestamp), length(rates))
  } else {
    effective_raw <- rep(NA_character_, length(rates))
  }

  exchange_rate <- ifelse(is.na(hbar_equivalent) | hbar_equivalent == 0,
    NA_real_,
    cent_equivalent / hbar_equivalent
  )

  tibble::tibble(
    rate_type = rate_type,
    hbar_equivalent = hbar_equivalent,
    cent_equivalent = cent_equivalent,
    exchange_rate = exchange_rate,
    expiration_time_seconds = expiration_seconds,
    expiration_timestamp = lubridate::as_datetime(expiration_seconds, tz = "UTC"),
    effective_timestamp = hadeda_parse_timestamp(effective_raw)
  )
}
