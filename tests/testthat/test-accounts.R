test_that("accounts_list paginates and parses records", {
  cfg <- hadeda_config(network = "testnet")
  page_one <- list(
    accounts = list(
      list(
        account = "0.0.1001",
        balance = list(balance = 100, timestamp = "1672531200.000000000"),
        deleted = FALSE,
        key = list(key = "302a300506032b6570032100abc")
      )
    ),
    links = list("next" = "accounts?cursor=next")
  )
  page_two <- list(
    accounts = list(
      list(
        account = "0.0.2002",
        balance = list(balance = 200, timestamp = "1672531210.000000000"),
        deleted = TRUE,
        key = list(key = "302a300506032b6570032100def")
      )
    ),
    links = list("next" = NULL)
  )

  with_mocked_bindings({
    tbl <- accounts_list(cfg, limit = 1, order = "asc")
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, "accounts")
    expect_equal(query$limit, 1)
    expect_equal(query$order, "asc")
    list(page_one, page_two)
  })

  expect_equal(nrow(tbl), 2)
  expect_true(all(c("account", "balance", "timestamp", "deleted", "public_key") %in% names(tbl)))
  expect_s3_class(tbl$timestamp, "POSIXct")
  expect_equal(tbl$balance, c(100, 200))
  expect_identical(tbl$deleted, c(FALSE, TRUE))
})

test_that("accounts_get returns single row tibble", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    account = "0.0.1234",
    balance = list(balance = 500, timestamp = "1672531300.000000000"),
    deleted = FALSE,
    key = list(key = "002a")
  )

  with_mocked_bindings({
    tbl <- accounts_get(cfg, "0.0.1234")
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "accounts/0.0.1234")
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$account, "0.0.1234")
  expect_equal(tbl$balance, 500)
})

test_that("accounts_balance unwraps balances payload", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    timestamp = "1672531350.000000000",
    balances = list(
      list(account = "0.0.1234", balance = 900, tokens = list(list(token_id = "0.0.7", balance = 1))),
      list(account = "0.0.4321", balance = 400, tokens = list())
    )
  )

  with_mocked_bindings({
    tbl <- accounts_balance(cfg, "0.0.1234", timestamp = "gt:1672531300.000000000Z")
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, "accounts/0.0.1234/balance")
    expect_equal(query$timestamp, "gt:1672531300.000000000Z")
    response
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$balance, c(900, 400))
  expect_type(tbl$tokens, "list")
  expect_s3_class(tbl$timestamp, "POSIXct")
})

test_that("accounts_create posts payload and parses response", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    accountId = "0.0.6001",
    evmAddress = "0xabc123",
    publicKey = "302a300506032b6570032100abc",
    privateKey = "302e020100300506032b657004220420def",
    mnemonic = c("alpha", "beta", "gamma")
  )

  with_mocked_bindings({
    tbl <- accounts_create(cfg, initial_balance = 10, memo = "demo")
  }, hadeda_rest_post = function(config, path, body = list()) {
    expect_identical(path, "accounts")
    expect_equal(body$initialBalance, 10)
    expect_equal(body$memo, "demo")
    response
  })

  expect_s3_class(tbl, "tbl_df")
  expect_equal(tbl$account, "0.0.6001")
  expect_equal(tbl$evm_address, "0xabc123")
  expect_equal(tbl$public_key, "302a300506032b6570032100abc")
  expect_equal(tbl$private_key, "302e020100300506032b657004220420def")
  expect_equal(tbl$mnemonic[[1]], c("alpha", "beta", "gamma"))
  expect_equal(tbl$response, list(response))
})
