#' Parse consensus timestamps
#'
#' Convert Hedera consensus timestamps to POSIXct values.
#'
#' @param x A character vector of consensus timestamps.
#'
#' @return A POSIXct vector in UTC.
#'
#' @examples
#' hadeda_parse_timestamp(c("1700000000.000000001", "1700000001.5"))
#'
#' @keywords internal
hadeda_parse_timestamp <- function(x) {
  if (length(x) == 0) {
    return(lubridate::as_datetime(numeric()))
  }

  parse_one <- function(value) {
    if (rlang::is_null(value) || is.na(value) || identical(value, "")) {
      return(NA_real_)
    }
    parts <- strsplit(value, "\\.", fixed = FALSE)[[1]]
    seconds <- suppressWarnings(as.numeric(parts[[1]]))
    nanos <- 0
    if (length(parts) > 1) {
      frac <- substr(parts[[2]], 1, 9)
      nanos <- suppressWarnings(as.numeric(paste0("0.", frac)))
    }
    if (is.na(seconds) || is.na(nanos)) {
      return(NA_real_)
    }
    seconds + nanos
  }

  seconds <- vapply(x, parse_one, numeric(1), USE.NAMES = FALSE)
  lubridate::as_datetime(seconds, tz = "UTC")
}
