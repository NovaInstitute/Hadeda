#' Ensure a Hedera protobuf bundle is available for Hadeda
#'
#' Hedera publishes versioned protobuf definitions that Hadeda can use to build
#' gRPC clients. This helper keeps a local copy of the bundle inside the
#' package's `data/` directory (or an appropriate user cache when the package is
#' installed) and refreshes it when a new version is requested.
#'
#' @param version Version tag to download, without the leading `v`. Defaults to
#'   `"0.47.0"`, matching the Hedera 0.47 release.
#' @param force Set to `TRUE` to re-download the bundle even if the requested
#'   version is already available locally.
#'
#' @return The path to the local protobuf bundle directory (invisible).
#'
#' @examples
#' \dontrun{
#'   hadeda_ensure_proto_bundle()
#' }
#'
#' @export
hadeda_ensure_proto_bundle <- function(version = "0.47.0", force = FALSE) {
  bundle_dir <- hadeda_proto_bundle_dir(create = FALSE)
  metadata <- file.path(bundle_dir, "VERSION")

  if (!isTRUE(force)) {
    current <- hadeda_proto_bundle_version()
    if (!is.na(current) && identical(current, version)) {
      cli::cli_inform(c("i" = sprintf("Hedera protobuf bundle %s already available", version)))
      return(invisible(bundle_dir))
    }
  }

  cli::cli_inform(c("i" = sprintf("Fetching Hedera protobuf bundle %s", version)))
  download <- hadeda_download_proto_bundle(version)
  on.exit(download$cleanup(), add = TRUE)
  downloaded <- download$path

  if (dir.exists(bundle_dir)) {
    unlink(bundle_dir, recursive = TRUE)
  }
  dir.create(dirname(bundle_dir), recursive = TRUE, showWarnings = FALSE)

  moved <- file.rename(downloaded, bundle_dir)
  if (!isTRUE(moved)) {
    dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
    files <- list.files(downloaded, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    copied <- vapply(
      files,
      function(path) file.copy(path, bundle_dir, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE),
      logical(1)
    )
    if (length(copied) && !all(copied)) {
      cli::cli_abort("Failed to copy extracted protobuf files into the data directory.")
    }
  }

  writeLines(version, metadata)
  invisible(bundle_dir)
}

#' Report the version of the local Hedera protobuf bundle, if any
#'
#' @return A single string containing the downloaded version, or `NA` when no
#'   bundle is available.
#'
#' @examples
#' hadeda_proto_bundle_version()
#'
#' @export
hadeda_proto_bundle_version <- function() {
  bundle_dir <- hadeda_proto_bundle_dir(create = FALSE)
  metadata <- file.path(bundle_dir, "VERSION")

  if (!file.exists(metadata)) {
    return(NA_character_)
  }

  version <- readLines(metadata, warn = FALSE)
  version <- version[seq_len(min(length(version), 1L))]
  version <- trimws(version)
  if (!length(version) || !nzchar(version)) {
    return(NA_character_)
  }
  version
}

hadeda_proto_bundle_dir <- function(create = TRUE) {
  data_dir <- hadeda_package_data_dir(create = create)
  bundle_dir <- file.path(data_dir, "hedera-protobufs")
  if (isTRUE(create)) {
    dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  }
  bundle_dir
}

hadeda_package_data_dir <- function(create = TRUE) {
  pkg_name <- "hadeda"
  pkg_path <- NULL

  if (pkg_name %in% loadedNamespaces()) {
    pkg_path <- getNamespaceInfo(asNamespace(pkg_name), "path")
  }

  if (is.null(pkg_path) || !nzchar(pkg_path)) {
    pkg_path <- system.file(package = pkg_name)
  }

  if (!nzchar(pkg_path)) {
    pkg_path <- hadeda_locate_package_root()
  }

  if (!nzchar(pkg_path)) {
    cli::cli_abort("Unable to determine a writable data directory for hadeda.")
  }

  writable <- tryCatch(file.access(pkg_path, 2) == 0, warning = function(...) FALSE, error = function(...) FALSE)
  data_dir <- if (isTRUE(writable)) {
    file.path(pkg_path, "data")
  } else {
    tools::R_user_dir(pkg_name, which = "data")
  }

  if (isTRUE(create)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  data_dir
}

hadeda_locate_package_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    desc_path <- file.path(path, "DESCRIPTION")
    if (file.exists(desc_path)) {
      pkg_name <- tryCatch(
        read.dcf(desc_path, fields = "Package")[1],
        error = function(err) NA_character_
      )
      if (identical(pkg_name, "hadeda")) {
        return(path)
      }
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      break
    }
    path <- parent
  }

  stop("Unable to locate the Hadeda package root.")
}

hadeda_download_proto_bundle <- function(version) {
  url <- sprintf(
    "https://github.com/hashgraph/hedera-protobufs/archive/refs/tags/v%s.zip",
    version
  )

  temp_file <- tempfile(fileext = ".zip")
  temp_dir <- tempfile("hedera-proto-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  utils::download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)
  utils::unzip(temp_file, exdir = temp_dir)

  extracted <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
  if (length(extracted) != 1L) {
    cli::cli_abort("Unexpected archive contents while extracting protobuf bundle.")
  }

  list(
    path = extracted,
    cleanup = function() {
      if (file.exists(temp_file)) unlink(temp_file)
      if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
    }
  )
}
