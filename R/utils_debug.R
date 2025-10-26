# Internal helpers for conditional debugging output -------------------------

hadeda_debug_enabled <- function() {
  option_enabled <- isTRUE(getOption("hadeda.debug"))
  env_value <- tolower(Sys.getenv("HADEDA_DEBUG", ""))
  env_enabled <- nzchar(env_value) && env_value %in% c("1", "true", "yes", "on")

  option_enabled || env_enabled
}

hadeda_debug <- function(message, ...) {
  if (!hadeda_debug_enabled()) {
    return(invisible(NULL))
  }

  formatted <- if (length(list(...))) {
    do.call(sprintf, c(list(message), list(...)))
  } else {
    message
  }

  formatted <- hadeda_escape_cli_braces(formatted)
  cli::cli_inform(c("debug" = formatted))
  invisible(NULL)
}

hadeda_debug_list <- function(title, values) {
  if (!hadeda_debug_enabled()) {
    return(invisible(NULL))
  }

  summary <- paste(utils::capture.output(str(values)), collapse = "\n")
  hadeda_debug("%s\n%s", title, summary)
}

hadeda_escape_cli_braces <- function(text) {
  text <- gsub("}", "}}", text, fixed = TRUE)
  gsub("{", "{{", text, fixed = TRUE)
}
