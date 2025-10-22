#' Read gRPC service definitions from an absolute proto path
#'
#' The helper wraps [`grpc::read_services()`][grpc::read_services] to work
#' with `.proto` files located outside the current working directory. It reads
#' the protobuf descriptors using [RProtoBuf::readProtoFiles()] with the fully
#' qualified path before parsing the service declarations, which avoids the
#' "none of the files exist" error triggered by relative lookups.
#'
#' @param file Path to the `.proto` file that contains service declarations.
#' @param package Package name passed to [RProtoBuf::readProtoFiles()]. Set to
#'   `NULL` to suppress package lookup.
#' @param pattern File name pattern forwarded to
#'   [RProtoBuf::readProtoFiles()]. When `NULL`, the default expression is used.
#' @param lib.loc Library path vector forwarded to
#'   [RProtoBuf::readProtoFiles()].
#'
#' @return A named list describing the RPC stubs in the `.proto` definition.
#'   The structure matches the output produced by `grpc::read_services()` so it
#'   can be supplied directly to `grpc::grpc_client()`.
#' @export
hadeda_read_services2 <- function(file,
                                  package = "RProtoBuf",
                                  pattern = "\\.proto$",
                                  lib.loc = NULL) {
  rlang::check_installed("RProtoBuf", reason = "for loading protobuf service definitions")

  file_path <- normalizePath(file, mustWork = TRUE)

  proto_args <- list(files = file_path)
  if (!is.null(package)) {
    proto_args$package <- package
  }
  if (!is.null(pattern)) {
    proto_args$pattern <- pattern
  }
  if (!is.null(lib.loc)) {
    proto_args$lib.loc <- lib.loc
  }

  do.call(RProtoBuf::readProtoFiles, proto_args)

  tokens <- hadeda_tokenise_proto(file_path)
  hadeda_parse_service_tokens(tokens)
}

hadeda_tokenise_proto <- function(file_path) {
  text <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
  text <- gsub("/\\*.*?\\*/", " ", text, perl = TRUE)
  text <- gsub("//.*", "", text)
  pieces <- strsplit(text, "(?=[{}();])|\\s+", perl = TRUE)
  tokens <- unlist(pieces, use.names = FALSE)
  tokens[nzchar(tokens)]
}

hadeda_parse_service_tokens <- function(tokens) {
  pkg <- ""
  cursor <- 1L
  services <- list()

  qualify <- function(name) {
    if (!nzchar(pkg) || grepl("\\.", name, fixed = TRUE)) {
      name
    } else {
      paste(pkg, name, sep = ".")
    }
  }

  advance_until <- function(target) {
    while (cursor <= length(tokens) && tokens[cursor] != target) {
      cursor <<- cursor + 1L
    }
  }

  while (cursor <= length(tokens)) {
    token <- tokens[cursor]

    if (identical(token, "package") && cursor < length(tokens)) {
      pkg <- tokens[cursor + 1L]
      cursor <- cursor + 2L
      next
    }

    if (identical(token, "service") && cursor < length(tokens)) {
      service_name <- tokens[cursor + 1L]
      cursor <- cursor + 2L
      advance_until("{")
      cursor <- cursor + 1L

      while (cursor <= length(tokens) && !identical(tokens[cursor], "}")) {
        if (!identical(tokens[cursor], "rpc") || cursor >= length(tokens)) {
          cursor <- cursor + 1L
          next
        }

        rpc_name <- tokens[cursor + 1L]
        cursor <- cursor + 2L

        advance_until("(")
        cursor <- cursor + 1L
        request_stream <- FALSE
        if (cursor <= length(tokens) && identical(tokens[cursor], "stream")) {
          request_stream <- TRUE
          cursor <- cursor + 1L
        }
        request_type <- tokens[cursor]
        advance_until(")")
        cursor <- cursor + 1L

        advance_until("(")
        cursor <- cursor + 1L
        response_stream <- FALSE
        if (cursor <= length(tokens) && identical(tokens[cursor], "stream")) {
          response_stream <- TRUE
          cursor <- cursor + 1L
        }
        response_type <- tokens[cursor]
        advance_until(")")
        cursor <- cursor + 1L

        while (cursor <= length(tokens) && !(tokens[cursor] %in% c(";", "{"))) {
          cursor <- cursor + 1L
        }

        if (cursor <= length(tokens) && identical(tokens[cursor], "{")) {
          depth <- 1L
          cursor <- cursor + 1L
          while (cursor <= length(tokens) && depth > 0L) {
            if (identical(tokens[cursor], "{")) {
              depth <- depth + 1L
            } else if (identical(tokens[cursor], "}")) {
              depth <- depth - 1L
            }
            cursor <- cursor + 1L
          }
        } else if (cursor <= length(tokens) && identical(tokens[cursor], ";")) {
          cursor <- cursor + 1L
        }

        services[[rpc_name]] <- list(
          f = I,
          RequestType = list(
            name = request_type,
            stream = request_stream,
            proto = qualify(request_type)
          ),
          ResponseType = list(
            name = response_type,
            stream = response_stream,
            proto = qualify(response_type)
          ),
          name = sprintf("/%s%s/%s", if (nzchar(pkg)) paste0(pkg, ".") else "", service_name, rpc_name)
        )
      }
    }

    cursor <- cursor + 1L
  }

  services
}
