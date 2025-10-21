test_that("hadeda_auth_config injects operator credentials", {
  old_id <- Sys.getenv("HADEDA_OPERATOR_ID", unset = NA)
  old_key <- Sys.getenv("HADEDA_OPERATOR_KEY", unset = NA)
  on.exit({
    if (is.na(old_id)) Sys.unsetenv("HADEDA_OPERATOR_ID") else Sys.setenv(HADEDA_OPERATOR_ID = old_id)
    if (is.na(old_key)) Sys.unsetenv("HADEDA_OPERATOR_KEY") else Sys.setenv(HADEDA_OPERATOR_KEY = old_key)
  }, add = TRUE)

  Sys.setenv(
    HADEDA_OPERATOR_ID = "0.0.5005",
    HADEDA_OPERATOR_KEY = "-----BEGIN PRIVATE KEY-----\nfake\n-----END PRIVATE KEY-----"
  )

  cfg <- hadeda_auth_config(signer = FALSE)

  expect_equal(cfg$network, "testnet")
  expect_equal(cfg$default_transport, "grpc")
  expect_equal(cfg$grpc$operator_account_id, "0.0.5005")
  expect_equal(
    cfg$grpc$operator_private_key,
    "-----BEGIN PRIVATE KEY-----\nfake\n-----END PRIVATE KEY-----"
  )
})

test_that("hadeda_auth_config honours explicit overrides", {
  cfg <- hadeda_auth_config(
    network = "previewnet",
    operator_id = "0.0.1234",
    operator_key = "key",
    grpc = list(custom = TRUE, sign_transaction = function(...) NULL),
    signer = FALSE
  )

  expect_equal(cfg$network, "previewnet")
  expect_true(cfg$grpc$custom)
  expect_identical(cfg$grpc$sign_transaction("ignored"), NULL)
})
