test_that("transactions_list honours filters and parses payload", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    list(
      transactions = list(
        list(
          transaction_id = "0.0.1001-1672531200-000000001",
          consensus_timestamp = "1672531201.123456789",
          name = "CRYPTOTRANSFER",
          valid_start_timestamp = "1672531200.000000000",
          charged_tx_fee = 123
        ),
        list(
          transaction_id = "0.0.1002-1672531210-000000001",
          consensus_timestamp = "1672531211.000000000",
          name = "CONSENSUSSUBMITMESSAGE",
          valid_start_timestamp = "1672531210.000000000",
          charged_tx_fee = 234
        )
      ),
      links = list("next" = NULL)
    )
  )

  with_mocked_bindings({
    tbl <- transactions_list(
      cfg,
      limit = 25,
      order = "desc",
      transaction_type = "CRYPTOTRANSFER",
      account_id = "0.0.1001",
      timestamp = "gte:1672531200.000000000Z"
    )
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, "transactions")
    expect_equal(query$limit, 25)
    expect_equal(query$order, "desc")
    expect_equal(query$transactiontype, "CRYPTOTRANSFER")
    expect_equal(query$`account.id`, "0.0.1001")
    expect_equal(query$timestamp, "gte:1672531200.000000000Z")
    response
  })

  expect_equal(nrow(tbl), 2)
  expect_true(all(c("transaction_id", "consensus_timestamp", "name", "valid_start", "charged_tx_fee") %in% names(tbl)))
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
  expect_equal(tbl$charged_tx_fee, c(123, 234))
})

test_that("transactions_get returns single row", {
  cfg <- hadeda_config(network = "testnet")
  response <- list(
    transaction_id = "0.0.1001-1672531200-000000001",
    consensus_timestamp = "1672531201.123456789",
    name = "CRYPTOTRANSFER",
    valid_start_timestamp = "1672531200.000000000",
    charged_tx_fee = 123
  )

  with_mocked_bindings({
    tbl <- transactions_get(cfg, response$transaction_id)
  }, hadeda_rest_get = function(config, path, query = list()) {
    expect_identical(path, paste0("transactions/", response$transaction_id))
    response
  })

  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$transaction_id, response$transaction_id)
})

test_that("topics_messages returns parsed tibble", {
  cfg <- hadeda_config(network = "testnet")
  topic_id <- "0.0.2000"
  responses <- list(
    list(
      messages = list(
        list(
          topic_id = topic_id,
          consensus_timestamp = "1672531400.000000000",
          message = "hello",
          running_hash = "abcd",
          sequence_number = 1
        )
      ),
      links = list("next" = "topics/0.0.2000/messages?sequencenumber=gt:1")
    ),
    list(
      messages = list(
        list(
          topic_id = topic_id,
          consensus_timestamp = "1672531500.000000000",
          message = "world",
          running_hash = "efgh",
          sequence_number = 2
        )
      ),
      links = list("next" = NULL)
    )
  )

  with_mocked_bindings({
    tbl <- topics_messages(cfg, topic_id, limit = 1, order = "asc", sequencenumber = "gte:1")
  }, hadeda_rest_paginate = function(config, path, query) {
    expect_identical(path, paste0("topics/", topic_id, "/messages"))
    expect_equal(query$limit, 1)
    expect_equal(query$order, "asc")
    expect_equal(query$sequencenumber, "gte:1")
    responses
  })

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$sequence_number, c(1, 2))
  expect_equal(tbl$message, c("hello", "world"))
})
