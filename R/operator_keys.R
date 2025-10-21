#' Convert a Hedera operator private key from DER hex to PEM
#'
#' Hedera's portal and SDK tooling often surface Ed25519 operator keys as
#' hexadecimal DER blobs (for example
#' `"302e020100300506032b657004220420e638e087c0dfe201b3519bd442033eba27b961a5fc969a7f4911b13ea2315769"`).
#' Hadeda's gRPC helpers expect PEM content instead. This helper decodes the
#' hex representation, parses it with `openssl`, and returns the PEM encoded
#' private key text so it can be written to disk or supplied directly to
#' [hadeda_grpc_ed25519_signer()].
#'
#' @param private_key_hex A character scalar containing the DER bytes expressed
#'   as hexadecimal.
#'
#' @return A character scalar containing the PEM encoded private key.
#'
#' @examples
#' \dontrun{
#' hadeda_operator_key_from_hex("302e020100300506032b657004220420e638e087c0dfe201b3519bd442033eba27b961a5fc969a7f4911b13ea2315769")
#' }
#'
#' @export
hadeda_operator_key_from_hex <- function(private_key_hex) {
  rlang::check_installed("openssl", reason = "for Ed25519 key conversion support")

  if (!is.character(private_key_hex) || length(private_key_hex) != 1L) {
    cli::cli_abort("`private_key_hex` must be a character scalar.")
  }

  hex <- gsub("\\s+", "", private_key_hex)
  if (!nzchar(hex)) {
    cli::cli_abort("`private_key_hex` cannot be empty.")
  }

  if ((nchar(hex) %% 2L) != 0L || !grepl("^[0-9a-fA-F]+$", hex)) {
    cli::cli_abort(
      c(
        "Invalid hex-encoded operator key",
        "x" = hex,
        "i" = "Supply the DER bytes as a continuous hexadecimal string."
      )
    )
  }

  byte_values <- strtoi(
    substring(hex, seq.int(1L, nchar(hex), 2L), seq.int(2L, nchar(hex), 2L)),
    base = 16L
  )

  if (anyNA(byte_values)) {
    cli::cli_abort("`private_key_hex` contains invalid hexadecimal bytes.")
  }

  raw_bytes <- as.raw(byte_values)

  key <- tryCatch(
    openssl::read_key(raw_bytes),
    error = function(err) {
      cli::cli_abort(
        c(
          "Failed to parse Ed25519 key from DER bytes",
          "x" = conditionMessage(err),
          "i" = "Ensure the hex string represents a PKCS#8 Ed25519 private key."
        )
      )
    }
  )

  pem <- openssl::write_pem(key)
  if (!nzchar(pem)) {
    cli::cli_abort("Failed to convert Ed25519 key into PEM format.")
  }
  pem
}

#' Determine the default location for Hadeda operator keys
#'
#' Hadeda stores operator keys in an application-specific configuration
#' directory. On Unix-like systems the default is `$XDG_CONFIG_HOME/hadeda`,
#' falling back to `~/.config/hadeda` when the environment variable is unset. On
#' Windows the directory resolves to `%APPDATA%\\hadeda`.
#'
#' @param config_home Optional override for the base configuration directory.
#' @param filename File name to append to the configuration directory. Defaults
#'   to `"operator_ed25519.pem"`.
#'
#' @return An expanded file path pointing to the suggested key location.
#'
#' @examples
#' hadeda_operator_key_path()
#'
#' @export
hadeda_operator_key_path <- function(config_home = NULL, filename = "operator_ed25519.pem") {
  if (!is.null(config_home)) {
    base_dir <- config_home
  } else if (.Platform$OS.type == "windows") {
    base_dir <- Sys.getenv("APPDATA")
    if (!nzchar(base_dir)) {
      base_dir <- file.path(path.expand("~"), "AppData", "Roaming")
    }
  } else {
    base_dir <- Sys.getenv("XDG_CONFIG_HOME")
    if (!nzchar(base_dir)) {
      base_dir <- file.path(path.expand("~"), ".config")
    }
  }

  path.expand(file.path(base_dir, "hadeda", filename))
}

#' Persist an operator key to disk and configure Hadeda
#'
#' This helper accepts a PEM string or DER hex representation of an Ed25519
#' operator key, writes it to the Hadeda configuration directory, and optionally
#' updates `HADEDA_OPERATOR_KEY` so that gRPC helpers can locate it. Existing
#' files are preserved unless `overwrite = TRUE`.
#'
#' @param private_key Either PEM text or DER bytes encoded as a hexadecimal
#'   string.
#' @param path Destination path for the key. Defaults to
#'   [hadeda_operator_key_path()].
#' @param overwrite Logical indicating whether an existing file may be replaced.
#' @param set_env Logical indicating whether to set `HADEDA_OPERATOR_KEY` to the
#'   written file path.
#' @param passphrase Optional passphrase used to encrypt the PEM file on disk.
#'
#' @return The path to the written PEM file (invisibly).
#'
#' @examples
#' \dontrun{
#' hadeda_write_operator_key(
#'   "302e020100300506032b657004220420e638e087c0dfe201b3519bd442033eba27b961a5fc969a7f4911b13ea2315769"
#' )
#' }
#'
#' @export
hadeda_write_operator_key <- function(private_key,
                                      path = hadeda_operator_key_path(),
                                      overwrite = FALSE,
                                      set_env = TRUE,
                                      passphrase = NULL) {
  rlang::check_installed("openssl", reason = "for Ed25519 key conversion support")

  if (!is.character(private_key) || length(private_key) != 1L) {
    cli::cli_abort("`private_key` must be a character scalar.")
  }

  pem <- hadeda_normalise_operator_key(private_key)
  destination <- path.expand(path)

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(destination) && !isTRUE(overwrite)) {
    cli::cli_abort(
      c(
        "Destination already exists",
        "x" = destination,
        "i" = "Pass `overwrite = TRUE` to replace the existing PEM file."
      )
    )
  }

  key <- tryCatch(
    openssl::read_key(pem),
    error = function(err) {
      cli::cli_abort(
        c(
          "Failed to parse Ed25519 key",
          "x" = conditionMessage(err),
          "i" = "Ensure the provided key is valid PEM or DER hex."
        )
      )
    }
  )

  openssl::write_pem(key, path = destination, password = passphrase)

  if (.Platform$OS.type != "windows") {
    tryCatch(
      Sys.chmod(destination, mode = "0600"),
      warning = function(...) NULL,
      error = function(...) NULL
    )
  }

  if (isTRUE(set_env)) {
    Sys.setenv(HADEDA_OPERATOR_KEY = destination)
  }

  invisible(destination)
}

hadeda_normalise_operator_key <- function(key_text) {
  trimmed <- trimws(key_text)
  if (!nzchar(trimmed)) {
    cli::cli_abort("Operator key cannot be empty.")
  }

  if (grepl("-----BEGIN ", trimmed, fixed = TRUE)) {
    return(trimmed)
  }

  hadeda_operator_key_from_hex(trimmed)
}
