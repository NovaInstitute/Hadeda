#' Read gRPC service definitions from an absolute proto path
#'
#' The helper wraps [`grpc::read_services()`][grpc::read_services] to work
#' with `.proto` files located outside the current working directory. It reads
#' the protobuf descriptors using [RProtoBuf::readProtoFiles()] or
#' [RProtoBuf::readProtoFiles2()] before parsing the service declarations,
#' which avoids the "none of the files exist" error triggered by relative
#' lookups.
#'
#' @param file Path to the `.proto` file that contains service declarations.
#' @param package Package name passed to [RProtoBuf::readProtoFiles()]. Defaults
#'   to `"RProtoBuf"` when omitted. Set to `NULL` to suppress package lookup.
#' @param pattern File name pattern forwarded to
#'   [RProtoBuf::readProtoFiles()]. When `NULL`, the default expression is used.
#' @param lib.loc Library path vector forwarded to
#'   [RProtoBuf::readProtoFiles()]. Defaults to `NULL`.
#' @param proto_path Character vector of additional import directories passed to
#'   the `protoPath` argument of [RProtoBuf::readProtoFiles2()]. Files must live
#'   inside one of these directories so that relative imports such as
#'   `google/protobuf/wrappers.proto` can be resolved. When supplied, the helper
#'   automatically appends any directories discovered by
#'   [hadeda_find_protobuf_include()].
#'
#' @return A named list describing the RPC stubs in the `.proto` definition.
#'   The structure matches the output produced by `grpc::read_services()` so it
#'   can be supplied directly to `grpc::grpc_client()`.
#' @export
hadeda_read_services2 <- function(file,
                                  package,
                                  pattern = "\\.proto$",
                                  lib.loc,
                                  proto_path = NULL) {
  rlang::check_installed("RProtoBuf", reason = "for loading protobuf service definitions")

  if (missing(package)) {
    package <- "RProtoBuf"
    package_supplied <- FALSE
  } else {
    package_supplied <- TRUE
  }

  if (missing(lib.loc)) {
    lib.loc <- NULL
    lib_loc_supplied <- FALSE
  } else {
    lib_loc_supplied <- TRUE
  }

  file_path <- normalizePath(file, winslash = "/", mustWork = TRUE)

  if (is.null(proto_path)) {
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
  } else {
    if ((package_supplied && !is.null(package)) || (lib_loc_supplied && !is.null(lib.loc))) {
      cli::cli_abort(c(
        "x" = "`package` and `lib.loc` are not supported when `proto_path` is supplied.",
        "i" = "Pass search directories via `proto_path` and supply proto files relative to those roots."
      ))
    }

    proto_path <- hadeda_merge_google_protos(proto_path)
    proto_roots <- normalizePath(proto_path, winslash = "/", mustWork = TRUE)
    file_relative <- hadeda_relativize_proto(file_path, proto_roots)

    proto_args <- list(files = file_relative, protoPath = proto_roots)
    if (!is.null(pattern)) {
      proto_args$pattern <- pattern
    }

    do.call(RProtoBuf::readProtoFiles2, proto_args)
  }

  tokens <- hadeda_tokenise_proto(file_path)
  hadeda_parse_service_tokens(tokens)
}

hadeda_merge_google_protos <- function(proto_path) {
  google_roots <- hadeda_find_protobuf_include()
  unique(c(proto_path, google_roots))
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

hadeda_relativize_proto <- function(file_path, proto_roots) {
  if (!length(proto_roots)) {
    cli::cli_abort("`proto_roots` must contain at least one directory.")
  }

  for (root in proto_roots) {
    prefix <- paste0(root, "/")
    if (identical(root, file_path)) {
      return(basename(file_path))
    }
    if (startsWith(file_path, prefix)) {
      relative <- substr(file_path, nchar(prefix) + 1L, nchar(file_path))
      return(relative)
    }
  }

  cli::cli_abort(c(
    "x" = "File {file_path} is not located in any of the supplied proto search paths.",
    "i" = "Add its parent directory to `proto_path`."
  ))
}
