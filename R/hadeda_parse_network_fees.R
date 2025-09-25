#' Parse Hedera network fee schedules
#'
#' Convert fee schedule payloads from the Mirror Node API into tidy tibbles.
#'
#' @param payload A list describing current and next fee schedules.
#'
#' @return A tibble with one row per schedule period.
#'
#' @keywords internal
hadeda_parse_network_fees <- function(payload) {
  schedules <- purrr::compact(list(
    list(
      type = "current",
      data = payload$current_fee_schedule %||% payload$currentFeeSchedule
    ),
    list(
      type = "next",
      data = payload$next_fee_schedule %||% payload$nextFeeSchedule
    )
  ))

  if (length(schedules) == 0) {
    return(tibble::tibble(
      schedule_type = character(),
      collected_timestamp = lubridate::as_datetime(numeric(), tz = "UTC"),
      period_start = lubridate::as_datetime(numeric(), tz = "UTC"),
      period_end = lubridate::as_datetime(numeric(), tz = "UTC"),
      expiry_time = lubridate::as_datetime(numeric(), tz = "UTC"),
      transaction_fee_schedule = list()
    ))
  }

  collected_raw <- payload$timestamp %||% NA_character_

  tibble::tibble(
    schedule_type = purrr::map_chr(schedules, "type"),
    collected_timestamp = hadeda_parse_timestamp(rep(collected_raw, length(schedules))),
    period_start = hadeda_parse_timestamp(purrr::map_chr(schedules, function(schedule) {
      timestamp <- schedule$data$timestamp
      if (is.null(timestamp)) {
        return(NA_character_)
      }
      timestamp$from %||% timestamp$start %||% NA_character_
    })),
    period_end = hadeda_parse_timestamp(purrr::map_chr(schedules, function(schedule) {
      timestamp <- schedule$data$timestamp
      if (is.null(timestamp)) {
        return(NA_character_)
      }
      timestamp$to %||% timestamp$end %||% NA_character_
    })),
    expiry_time = hadeda_parse_timestamp(purrr::map_chr(schedules, function(schedule) {
      schedule$data$expiry_time %||% schedule$data$expiryTime %||% NA_character_
    })),
    transaction_fee_schedule = purrr::map(schedules, function(schedule) {
      entries <- schedule$data$transaction_fee_schedule %||% schedule$data$transactionFeeSchedule %||% list()
      hadeda_parse_fee_schedule_entries(entries)
    })
  )
}

hadeda_parse_fee_schedule_entries <- function(entries) {
  if (length(entries) == 0) {
    return(tibble::tibble(
      hedera_functionality = character(),
      timestamp_from = lubridate::as_datetime(numeric(), tz = "UTC"),
      timestamp_to = lubridate::as_datetime(numeric(), tz = "UTC"),
      fees = list()
    ))
  }

  tibble::tibble(
    hedera_functionality = purrr::map_chr(entries, function(entry) {
      entry$hedera_functionality %||% entry$hederaFunctionality %||% NA_character_
    }),
    timestamp_from = hadeda_parse_timestamp(purrr::map_chr(entries, function(entry) {
      timestamp <- entry$timestamp
      if (is.null(timestamp)) {
        return(NA_character_)
      }
      timestamp$from %||% timestamp$start %||% NA_character_
    })),
    timestamp_to = hadeda_parse_timestamp(purrr::map_chr(entries, function(entry) {
      timestamp <- entry$timestamp
      if (is.null(timestamp)) {
        return(NA_character_)
      }
      timestamp$to %||% timestamp$end %||% NA_character_
    })),
    fees = purrr::map(entries, function(entry) {
      fees <- entry$fees %||% list()
      hadeda_parse_fee_entries(fees)
    })
  )
}

hadeda_parse_fee_entries <- function(entries) {
  if (length(entries) == 0) {
    return(tibble::tibble(
      sub_type = character(),
      fee_data = list()
    ))
  }

  tibble::tibble(
    sub_type = purrr::map_chr(entries, function(entry) {
      entry$sub_type %||% entry$subType %||% NA_character_
    }),
    fee_data = purrr::map(entries, function(entry) {
      data <- entry$fee_data %||% entry$feeData %||% list()
      hadeda_parse_fee_data(data)
    })
  )
}

hadeda_parse_fee_data <- function(data) {
  components <- list(
    network = data$network_data %||% data$networkData,
    node = data$node_data %||% data$nodeData,
    service = data$service_data %||% data$serviceData
  )

  tibble::tibble(
    component = names(components),
    fee_components = purrr::map(components, hadeda_parse_fee_components)
  )
}

hadeda_parse_fee_components <- function(components) {
  if (is.null(components) || length(components) == 0) {
    return(tibble::tibble(
      constant = numeric(),
      bpt = numeric(),
      vpt = numeric(),
      rbh = numeric(),
      sbh = numeric(),
      gas = numeric(),
      tv = numeric(),
      bpr = numeric(),
      sbpr = numeric(),
      min = numeric(),
      max = numeric()
    ))
  }

  fields <- c("constant", "bpt", "vpt", "rbh", "sbh", "gas", "tv", "bpr", "sbpr", "min", "max")
  values <- purrr::map_dbl(fields, function(field) {
    value <- components[[field]]
    if (is.null(value)) {
      return(NA_real_)
    }
    as.numeric(value)
  })

  tibble::as_tibble(as.list(stats::setNames(values, fields)))
}

