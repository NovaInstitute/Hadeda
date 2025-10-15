#' Hedera gRPC operator credentials from environment variables
#'
#' This helper reads the `HADEDA_OPERATOR_ID` and `HADEDA_OPERATOR_KEY`
#' environment variables and validates that they look plausible before they
#' are injected into a gRPC handler. It returns a list that mirrors the
#' structure expected inside `config$grpc`.
#'
#' @param account_id Optional account identifier to validate. Defaults to
#'   `Sys.getenv("HADEDA_OPERATOR_ID")`.
#' @param private_key Optional private key PEM. Defaults to
#'   `Sys.getenv("HADEDA_OPERATOR_KEY")`.
#'
#' @return A list with `operator_account_id` and `operator_private_key` entries.
#' @export
hadeda_grpc_env_credentials <- function(
  account_id = Sys.getenv("HADEDA_OPERATOR_ID"),
  private_key = Sys.getenv("HADEDA_OPERATOR_KEY")
) {
  if (!nzchar(account_id)) {
    cli::cli_abort(
      "Environment variable {.envvar HADEDA_OPERATOR_ID} is unset or empty."
    )
  }

  if (!grepl("^\\d+\\.\\d+\\.\\d+$", account_id)) {
    cli::cli_abort(
      c(
        "Invalid Hedera account identifier", "x" = account_id,
        "i" = "Account IDs use the shard.realm.num format, for example '0.0.5005'."
      )
    )
  }

  if (!nzchar(private_key)) {
    cli::cli_abort(
      "Environment variable {.envvar HADEDA_OPERATOR_KEY} is unset or empty."
    )
  }

  list(
    operator_account_id = account_id,
    operator_private_key = private_key
  )
}

#' Create an Ed25519 signing function for Hedera transactions
#'
#' Hedera networks use Ed25519 keys for operator accounts. This helper produces
#' a closure that Hadeda can use as the `sign_transaction` entry inside
#' `config$grpc`. The implementation relies on the `openssl` package to parse a
#' PEM encoded private key and generate raw signature bytes.
#'
#' @param private_key_pem A PEM string or file path containing an Ed25519
#'   private key. Defaults to `Sys.getenv("HADEDA_OPERATOR_KEY")`.
#' @param passphrase Optional passphrase required to decrypt the private key.
#'
#' @return A function that accepts a raw vector of transaction bytes and
#'   returns a raw vector containing the Ed25519 signature.
#' @export
hadeda_grpc_ed25519_signer <- function(
  private_key_pem = Sys.getenv("HADEDA_OPERATOR_KEY"),
  passphrase = NULL
) {
  rlang::check_installed("openssl", reason = "for Ed25519 signing support")

  if (!nzchar(private_key_pem)) {
    cli::cli_abort(
      "No private key supplied. Set {.envvar HADEDA_OPERATOR_KEY} or pass the PEM explicitly."
    )
  }

  pem_text <- if (file.exists(private_key_pem)) {
    readChar(private_key_pem, file.info(private_key_pem)$size)
  } else {
    private_key_pem
  }

  key <- tryCatch(
    openssl::read_key(pem_text, password = passphrase),
    error = function(err) {
      cli::cli_abort(
        c(
          "Failed to load Ed25519 private key", "x" = conditionMessage(err),
          "i" = "Ensure the key is PEM encoded and the passphrase (if any) is correct."
        )
      )
    }
  )

  signer <- function(tx_bytes) {
    if (is.character(tx_bytes)) {
      tx_bytes <- charToRaw(paste(tx_bytes, collapse = ""))
    }

    if (!is.raw(tx_bytes)) {
      cli::cli_abort("`tx_bytes` must be supplied as a raw vector or character scalar.")
    }

    openssl::signature_create(tx_bytes, key = key, hash = NULL)
  }

  attr(signer, "public_key") <- key$pubkey$data
  attr(signer, "private_key") <- key$data
  attr(signer, "pem") <- pem_text
  signer
}

#' Extract the public key bytes from a Hadeda gRPC signer
#'
#' `hadeda_grpc_ed25519_signer()` stores the Ed25519 public key as an attribute
#' on the returned closure. This helper retrieves the value and validates that it
#' has the expected 32 byte length.
#'
#' @param signer A function created by `hadeda_grpc_ed25519_signer()`.
#'
#' @return A raw vector containing the public key bytes.
#' @export
hadeda_grpc_signer_public_key <- function(signer) {
  pub <- attr(signer, "public_key", exact = TRUE)
  if (is.null(pub) || !is.raw(pub) || length(pub) != 32L) {
    cli::cli_abort("Signer does not contain an Ed25519 public key attribute.")
  }
  pub
}

#' Download Hedera protobuf definitions for local compilation
#'
#' Hedera publishes language-agnostic protobuf specifications that can be used
#' to generate gRPC clients. This helper downloads a tagged release of the
#' [`hashgraph/hedera-protobufs`](https://github.com/hashgraph/hedera-protobufs)
#' repository and extracts it into a local directory that you can point the
#' `grpc` package at.
#'
#' @param dest Directory where the protobuf definitions should be extracted.
#'   Defaults to `"proto"` in the current working directory.
#' @param version Release tag to download (without the leading `v`). Defaults to
#'   `"0.47.0"`, which matches the Hedera 0.47 network release.
#' @param overwrite Whether to overwrite an existing directory. Defaults to
#'   `FALSE`.
#'
#' @return The path to the extracted protobuf directory (invisible).
#' @export
hadeda_grpc_use_proto_bundle <- function(dest = "proto", version = "0.47.0", overwrite = FALSE) {
  url <- sprintf(
    "https://github.com/hashgraph/hedera-protobufs/archive/refs/tags/v%s.zip",
    version
  )

  dest_path <- dest
  if (!grepl("^(/|[A-Za-z]:|~|\\\\\\\\)", dest_path)) {
    dest_path <- file.path(getwd(), dest_path)
  }
  dest_path <- path.expand(dest_path)

  if (dir.exists(dest_path)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort(
        c(
          "Destination already exists", "x" = dest_path,
          "i" = "Pass `overwrite = TRUE` to refresh the bundle."
        )
      )
    }
    unlink(dest_path, recursive = TRUE)
  }

  parent_dir <- dirname(dest_path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }

  temp_file <- tempfile(fileext = ".zip")
  temp_dir <- tempfile("hedera-proto-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit({
    if (file.exists(temp_file)) unlink(temp_file)
    if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

  cli::cli_inform(c("i" = sprintf("Downloading Hedera protobufs %s", version)))
  utils::download.file(url, destfile = temp_file, mode = "wb", quiet = TRUE)

  utils::unzip(temp_file, exdir = temp_dir)
  extracted <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
  if (length(extracted) != 1L) {
    cli::cli_abort("Unexpected archive contents while extracting protobuf bundle.")
  }

  moved <- file.rename(extracted, dest_path)
  if (!isTRUE(moved)) {
    dir.create(dest_path, recursive = TRUE, showWarnings = FALSE)
    files <- list.files(extracted, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    copied <- vapply(
      files,
      function(path) file.copy(path, dest_path, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE),
      logical(1)
    )
    if (length(copied) && !all(copied)) {
      cli::cli_abort("Failed to copy extracted protobuf files into the destination directory.")
    }
  }

  invisible(normalizePath(dest_path))
}
