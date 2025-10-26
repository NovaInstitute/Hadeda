test_that("hadeda_grpc_env_credentials reads operator env vars", {
  old_id <- Sys.getenv("HADEDA_OPERATOR_ID", unset = NA)
  old_key <- Sys.getenv("HADEDA_OPERATOR_KEY", unset = NA)
  on.exit({
    if (is.na(old_id)) {
      Sys.unsetenv("HADEDA_OPERATOR_ID")
    } else {
      Sys.setenv(HADEDA_OPERATOR_ID = old_id)
    }
    if (is.na(old_key)) {
      Sys.unsetenv("HADEDA_OPERATOR_KEY")
    } else {
      Sys.setenv(HADEDA_OPERATOR_KEY = old_key)
    }
  })

  Sys.setenv(
    HADEDA_OPERATOR_ID = "0.0.1234",
    HADEDA_OPERATOR_KEY = "-----BEGIN PRIVATE KEY-----\nabc\n-----END PRIVATE KEY-----\n"
  )

  creds <- hadeda_grpc_env_credentials()
  expect_equal(creds$operator_account_id, "0.0.1234")
  expect_match(creds$operator_private_key, "BEGIN PRIVATE KEY")
  expect_match(creds$operator_private_key, "END PRIVATE KEY")
})

test_that("hadeda_grpc_env_credentials validates account identifiers", {
  old_id <- Sys.getenv("HADEDA_OPERATOR_ID", unset = NA)
  old_key <- Sys.getenv("HADEDA_OPERATOR_KEY", unset = NA)
  on.exit({
    if (is.na(old_id)) {
      Sys.unsetenv("HADEDA_OPERATOR_ID")
    } else {
      Sys.setenv(HADEDA_OPERATOR_ID = old_id)
    }
    if (is.na(old_key)) {
      Sys.unsetenv("HADEDA_OPERATOR_KEY")
    } else {
      Sys.setenv(HADEDA_OPERATOR_KEY = old_key)
    }
  })

  Sys.setenv(
    HADEDA_OPERATOR_ID = "not-an-account",
    HADEDA_OPERATOR_KEY = "-----BEGIN PRIVATE KEY-----\nabc\n-----END PRIVATE KEY-----\n"
  )

  expect_error(
    hadeda_grpc_env_credentials(),
    "Invalid Hedera account identifier"
  )
})

test_that("hadeda_grpc_env_credentials requires a private key", {
  old_id <- Sys.getenv("HADEDA_OPERATOR_ID", unset = NA)
  old_key <- Sys.getenv("HADEDA_OPERATOR_KEY", unset = NA)
  on.exit({
    if (is.na(old_id)) {
      Sys.unsetenv("HADEDA_OPERATOR_ID")
    } else {
      Sys.setenv(HADEDA_OPERATOR_ID = old_id)
    }
    if (is.na(old_key)) {
      Sys.unsetenv("HADEDA_OPERATOR_KEY")
    } else {
      Sys.setenv(HADEDA_OPERATOR_KEY = old_key)
    }
  })

  Sys.setenv(HADEDA_OPERATOR_ID = "0.0.1234", HADEDA_OPERATOR_KEY = "")

  expect_error(
    hadeda_grpc_env_credentials(),
    "HADEDA_OPERATOR_KEY"
  )
})

test_that("hadeda_grpc_env_credentials converts hex operator keys", {
  testthat::skip_if_not_installed("openssl")

  key <- openssl::ed25519_keygen()
  der <- openssl::write_der(key)
  hex <- paste(sprintf("%02x", as.integer(der)), collapse = "")

  old_id <- Sys.getenv("HADEDA_OPERATOR_ID", unset = NA)
  old_key <- Sys.getenv("HADEDA_OPERATOR_KEY", unset = NA)
  on.exit({
    if (is.na(old_id)) {
      Sys.unsetenv("HADEDA_OPERATOR_ID")
    } else {
      Sys.setenv(HADEDA_OPERATOR_ID = old_id)
    }
    if (is.na(old_key)) {
      Sys.unsetenv("HADEDA_OPERATOR_KEY")
    } else {
      Sys.setenv(HADEDA_OPERATOR_KEY = old_key)
    }
  })

  Sys.setenv(HADEDA_OPERATOR_ID = "0.0.9876", HADEDA_OPERATOR_KEY = hex)
  creds <- hadeda_grpc_env_credentials()

  expect_match(creds$operator_private_key, "BEGIN PRIVATE KEY")
  parsed <- openssl::read_key(creds$operator_private_key)
  expect_s3_class(parsed, "key")
  expect_identical(parsed$data, key$data)
})

test_that("hadeda_grpc_ed25519_signer signs payloads", {
  testthat::skip_if_not_installed("openssl")

  key <- openssl::ed25519_keygen()
  pem <- openssl::write_pem(key)
  signer <- hadeda_grpc_ed25519_signer(pem)

  payload <- charToRaw("hello, hedera")
  signature <- signer(payload)

  expect_match(pem, "BEGIN PRIVATE KEY")
  expect_type(signature, "raw")
  expect_true(length(signature) > 0)

  pub <- hadeda_grpc_signer_public_key(signer)
  expect_type(pub, "raw")
  expect_length(pub, 32L)
})

test_that("hadeda_grpc_ed25519_signer accepts hex input", {
  testthat::skip_if_not_installed("openssl")

  key <- openssl::ed25519_keygen()
  der <- openssl::write_der(key)
  hex <- paste(sprintf("%02x", as.integer(der)), collapse = "")

  signer <- hadeda_grpc_ed25519_signer(hex)
  payload <- charToRaw("hex support")
  signature <- signer(payload)

  expect_type(signature, "raw")
  expect_length(signature, 64L)
  expect_identical(attr(signer, "private_key", exact = TRUE), key$data)
})

test_that("hadeda_grpc_ed25519_signer exposes private key bytes", {
  testthat::skip_if_not_installed("openssl")

  key <- openssl::ed25519_keygen()
  pem <- openssl::write_pem(key)
  signer <- hadeda_grpc_ed25519_signer(pem)

  expect_identical(attr(signer, "private_key", exact = TRUE), key$data)
})

test_that("hadeda_grpc_signer_public_key rejects malformed signers", {
  signer <- function(raw) raw
  expect_error(
    hadeda_grpc_signer_public_key(signer),
    "Signer does not contain an Ed25519 public key attribute"
  )
})

test_that("hadeda gRPC configuration can host channel factories", {
  testthat::skip_if_not_installed("grpc")

  config <- hadeda_config(network = "testnet")
  expect_equal(config$grpc$host, "testnet.grpc.hedera.com")

  handler <- list(
    channel = function(target = "localhost:50211", opts = list()) {
      # christiaanpauw/grpc does not expose channel constructors. The client
      # expects the raw target string and opens the channel internally.
      if (!rlang::is_empty(opts)) {
        cli::cli_warn(
          "Channel options are ignored by the grpc client implementation."
        )
      }
      target
    }
  )

  config$grpc <- c(config$grpc, handler)
  expect_type(config$grpc$port, "integer")
  expect_true(is.list(config$grpc))
})

test_that("hadeda_grpc_use_proto_bundle copies when rename fails", {
  dest_dir <- file.path(tempdir(), paste0("hadeda-proto-", as.integer(Sys.time())))
  on.exit(unlink(dest_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode = "wb", quiet = TRUE) {
      file.create(destfile)
      invisible(0L)
    },
    unzip = function(zipfile, exdir) {
      bundle <- file.path(exdir, "hedera-protobufs-test")
      dir.create(file.path(bundle, "services"), recursive = TRUE, showWarnings = FALSE)
      writeLines("syntax = \"proto3\";", file.path(bundle, "services", "network_service.proto"))
      invisible(character())
    },
    .package = "utils"
  )

  testthat::local_mocked_bindings(
    file.rename = function(from, to) FALSE,
    .package = "base"
  )

  hadeda_grpc_use_proto_bundle(dest = dest_dir, version = "test", overwrite = TRUE)

  expect_true(dir.exists(dest_dir))
  expect_true(file.exists(file.path(dest_dir, "services", "network_service.proto")))
})
