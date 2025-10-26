skip_if_not_installed("openssl")

hex_key <- "302e020100300506032b657004220420e638e087c0dfe201b3519bd442033eba27b961a5fc969a7f4911b13ea2315769"

test_that("hadeda_operator_key_from_hex converts DER to PEM", {
  pem <- hadeda_operator_key_from_hex(hex_key)
  expect_match(pem, "BEGIN PRIVATE KEY")
  expect_match(pem, "END PRIVATE KEY")

  key <- openssl::read_key(pem)
  expect_s3_class(key, "key")
})

test_that("hadeda_operator_key_path builds platform-specific defaults", {
  tmp <- tempfile("config-home-")
  path <- hadeda_operator_key_path(config_home = tmp, filename = "custom.pem")
  expect_equal(path, file.path(tmp, "hadeda", "custom.pem"))
})

test_that("hadeda_write_operator_key persists PEM and updates environment", {
  tmpdir <- tempdir()
  dest <- file.path(tmpdir, "operator.pem")

  old <- Sys.getenv("HADEDA_OPERATOR_KEY")
  on.exit(Sys.setenv(HADEDA_OPERATOR_KEY = old), add = TRUE)

  invisible(hadeda_write_operator_key(hex_key, path = dest, set_env = TRUE, overwrite = TRUE))

  expect_equal(Sys.getenv("HADEDA_OPERATOR_KEY"), dest)
  expect_true(file.exists(dest))

  pem <- readChar(dest, file.info(dest)$size)
  expect_match(pem, "BEGIN PRIVATE KEY")

  key <- openssl::read_key(dest)
  expect_s3_class(key, "key")
})

test_that("hadeda_write_operator_key refuses to overwrite by default", {
  tmpfile <- tempfile(fileext = ".pem")
  old <- Sys.getenv("HADEDA_OPERATOR_KEY")
  on.exit(Sys.setenv(HADEDA_OPERATOR_KEY = old), add = TRUE)

  invisible(hadeda_write_operator_key(hex_key, path = tmpfile, set_env = FALSE, overwrite = TRUE))
  original <- readChar(tmpfile, file.info(tmpfile)$size)

  expect_warning(
    invisible(hadeda_write_operator_key(hex_key, path = tmpfile, set_env = FALSE)),
    "Destination already exists"
  )

  expect_equal(readChar(tmpfile, file.info(tmpfile)$size), original)
})
