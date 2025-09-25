test_that("integration: accounts_get retrieves a live account", {
  skip_if(Sys.getenv("HADEDA_TESTNET_ACCOUNT") == "", "Integration tests require HADEDA_TESTNET_ACCOUNT")
  skip_if(Sys.getenv("HADEDA_TESTNET_API_KEY") == "", "Integration tests require HADEDA_TESTNET_API_KEY")

  config <- hadeda_config(
    network = "testnet",
    rest = list(
      base_url = "https://testnet.hashio.io/api/v1",
      headers = list(`X-API-Key` = Sys.getenv("HADEDA_TESTNET_API_KEY"))
    ),
    default_transport = "rest"
  )

  account_id <- Sys.getenv("HADEDA_TESTNET_ACCOUNT")
  result <- accounts_get(config, account_id = account_id)

  expect_s3_class(result, "tbl_df")
  expect_gte(nrow(result), 1)
  expect_equal(result$account_id[[1]], account_id)
})



test_that("integration: consensus message submission acknowledges receipt", {
  skip_if(Sys.getenv("HADEDA_TESTNET_TOPIC") == "", "Integration tests require HADEDA_TESTNET_TOPIC")
  skip_if(Sys.getenv("HADEDA_TESTNET_OPERATOR") == "", "Integration tests require HADEDA_TESTNET_OPERATOR")
  skip_if(Sys.getenv("HADEDA_TESTNET_KEY") == "", "Integration tests require HADEDA_TESTNET_KEY")

  skip("Consensus integration requires a configured gRPC handler in config$grpc; provide one when running locally.")
})
