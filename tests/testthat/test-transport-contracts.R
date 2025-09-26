test_that("tokens_create returns consistent schema across transports", {
  skip_if_not_installed("mockery")

  rest_config <- hadeda_config(network = "testnet", default_transport = "rest")
  grpc_config <- hadeda_config(network = "testnet", default_transport = "rest")

  rest_response <- list(
    tokenId = "0.0.6001",
    transactionId = "0.0.5001-1700000000-000000123",
    status = "SUCCESS"
  )

  grpc_response <- list(
    transaction_id = "0.0.5001-1700000000-000000123",
    status = "OK",
    receipt = list(
      status = "SUCCESS",
      token_id = "0.0.6001"
    ),
    consensus_timestamp = "1700000000.123456789"
  )

  mockery::stub(tokens_create, "hadeda_rest_post", function(...) rest_response)
  mockery::stub(tokens_create, "hadeda_grpc_tokens_create", function(...) grpc_response)

  rest_tbl <- tokens_create(
    rest_config,
    name = "Hadeda Test",
    symbol = "HAD",
    treasury_account_id = "0.0.5001"
  )
  grpc_tbl <- tokens_create(
    grpc_config,
    name = "Hadeda Test",
    symbol = "HAD",
    treasury_account_id = "0.0.5001",
    .transport = "grpc"
  )

  expect_s3_class(rest_tbl, "tbl_df")
  expect_s3_class(grpc_tbl, "tbl_df")
  expect_identical(vctrs::vec_ptype(rest_tbl), vctrs::vec_ptype(grpc_tbl))
  expect_equal(rest_tbl$token_id, grpc_tbl$token_id)
  expect_snapshot(rest_tbl)
})

