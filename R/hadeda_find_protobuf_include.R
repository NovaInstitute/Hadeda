#' Locate the system Protocol Buffers include directory
#'
#' The `protobuf` compiler ships the standard Google protobuf definitions under
#' an `include/google/protobuf` directory. This helper searches a number of
#' platform-specific installation prefixes and environment variables to find
#' those headers so they can be added to `RProtoBuf`'s import path.
#'
#' The lookup honours the `PROTOBUF_INCLUDE` environment variable, attempts to
#' query Homebrew and `pkg-config` when available, and finally checks a set of
#' common installation directories on Unix and Windows.
#'
#' @return A character vector of include directories that contain
#'   `google/protobuf`. When no directories are found, the helper returns an
#'   empty character vector.
#'
#' @examples
#' hadeda_find_protobuf_include()
#'
#' @export
hadeda_find_protobuf_include <- function() {
  candidates <- character()

  env_path <- Sys.getenv("PROTOBUF_INCLUDE", unset = NA_character_)
  if (!is.na(env_path) && nzchar(env_path)) {
    env_dirs <- strsplit(env_path, split = .Platform$path.sep, fixed = TRUE)[[1]]
    candidates <- c(candidates, env_dirs)
  }

  brew <- Sys.which("brew")
  if (nzchar(brew)) {
    brew_prefix <- hadeda_system_stdout(brew, c("--prefix", "protobuf"))
    brew_prefix <- brew_prefix[nzchar(brew_prefix)]
    if (length(brew_prefix)) {
      candidates <- c(candidates, file.path(brew_prefix[[1]], "include"))
    }
  }

  pkg_cfg <- Sys.which("pkg-config")
  if (nzchar(pkg_cfg)) {
    pkg_include <- hadeda_system_stdout(pkg_cfg, c("--variable=includedir", "protobuf"))
    pkg_include <- pkg_include[nzchar(pkg_include)]
    if (length(pkg_include)) {
      candidates <- c(candidates, pkg_include[[1]])
    }
  }

  protoc <- Sys.which("protoc")
  if (nzchar(protoc)) {
    protoc_path <- tryCatch(normalizePath(protoc, winslash = "/", mustWork = TRUE), error = function(...) NA_character_)
    if (!is.na(protoc_path)) {
      prefix <- dirname(dirname(protoc_path))
      candidates <- c(candidates, file.path(prefix, "include"))
    }
  }

  win_prefixes <- unique(c(Sys.getenv("ProgramFiles"), Sys.getenv("ProgramFiles(x86)")))
  win_prefixes <- win_prefixes[nzchar(win_prefixes)]
  if (length(win_prefixes)) {
    windows_candidates <- file.path(win_prefixes, "protoc", "include")
    candidates <- c(candidates, windows_candidates)
  }

  unix_defaults <- c(
    "/usr/local/include",
    "/usr/include",
    "/opt/homebrew/include",
    "/opt/homebrew/opt/protobuf/include",
    "/opt/local/include"
  )
  candidates <- c(candidates, unix_defaults)

  candidates <- unique(candidates[nzchar(candidates)])
  resolved <- vapply(candidates, hadeda_normalise_google_proto, character(1), USE.NAMES = FALSE)
  resolved <- resolved[nzchar(resolved)]
  if (!length(resolved)) {
    return(character())
  }

  normalizePath(unique(resolved), winslash = "/", mustWork = FALSE)
}

hadeda_normalise_google_proto <- function(path) {
  if (!dir.exists(path)) {
    return("")
  }

  if (dir.exists(file.path(path, "google", "protobuf"))) {
    return(path)
  }

  if (identical(basename(path), "protobuf") && identical(basename(dirname(path)), "google")) {
    parent <- dirname(dirname(path))
    if (dir.exists(file.path(parent, "google", "protobuf"))) {
      return(parent)
    }
  }

  ""
}

hadeda_system_stdout <- function(command, args) {
  suppressWarnings(
    tryCatch(
      system2(command, args, stdout = TRUE, stderr = FALSE),
      warning = function(...) character(),
      error = function(...) character()
    )
  )
}
