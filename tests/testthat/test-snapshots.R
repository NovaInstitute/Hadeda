test_that("account parsing remains stable", {
  records <- list(list(
    account = "0.0.1001",
    balance = list(balance = 100L, timestamp = "1700000000.000000000"),
    deleted = FALSE,
    key = list(key = "302a300506032b6570032100...")
  ))

  parsed <- hadeda_parse_accounts(records)
  expect_snapshot(parsed)
})

test_that("token balance parsing remains stable", {
  payload <- list(list(
    account = "0.0.1001",
    balance = 2500000000,
    decimals = 8
  ))

  parsed <- hadeda_parse_token_balances(payload)
  expect_snapshot(parsed)
})

test_that("consensus response parsing normalises chunks", {
  responses <- list(
    list(
      transaction_id = "0.0.5001-1700000000-000000123",
      precheck_code = "OK",
      receipt = list(
        status = "SUCCESS",
        sequence_number = 42,
        consensus_timestamp = "1700000000.123456789"
      ),
      sequence_number = 42
    ),
    list(
      transactionId = "0.0.5001-1700000000-000000124",
      status = "BUSY",
      receipt = list(status = "BUSY")
    )
  )

  parsed <- hadeda_parse_consensus_responses("0.0.9001", responses)
  expect_snapshot(parsed)
})
