hadeda_drop_null <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  is_null <- vapply(x, is.null, logical(1))
  x[!is_null]
}

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
      "REST transport is not supported for this helper.",
      class = "hadeda_unsupported_transport",
      call = caller
    )
  }
  if (choice == "grpc" && !grpc_supported) {
    cli::cli_abort(
      "gRPC transport is not supported for this helper.",
      class = "hadeda_unsupported_transport",
      call = caller
    )
  }

  choice
}

hadeda_require_rest <- function(config, caller = rlang::caller_env()) {
  rest <- config$rest
  if (is.null(rest) || !is.list(rest) || is.null(rest$base_url)) {
    cli::cli_abort(
      "REST configuration is missing a `base_url`.\nDid you create the configuration with `hadeda_config()`?",
      class = "hadeda_missing_rest_config",
      call = caller
    )
  }
  rest
}

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
