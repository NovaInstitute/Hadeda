test_that("hadeda_require_grpc validates configuration", {
  cfg <- hadeda_config(network = "testnet")
  expect_equal(hadeda:::hadeda_require_grpc(cfg)$host, cfg$grpc$host)

  broken <- cfg
  broken$grpc <- list()
  expect_error(hadeda:::hadeda_require_grpc(broken), "gRPC configuration")
})

test_that("consensus_submit_message validates transport", {
  cfg <- hadeda_config(network = "testnet")
  expect_error(consensus_submit_message(cfg, "0.0.1000", "hi"), "gRPC transport")
})

test_that("consensus_submit_message supports single chunk submissions", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"

  payload <- "hello"
  response <- list(
    transaction_id = "0.0.1000-1700000000-000000000",
    status = "OK",
    receipt = list(status = "SUCCESS"),
    sequenceNumber = 42,
    consensusTimestamp = "1700000000.000000000"
  )

  calls <- list()
  with_mocked_bindings({
    tbl <- consensus_submit_message(
      cfg,
      topic_id = "0.0.2000",
      message = payload,
      message_type = "text",
      chunk_size = 1024L
    )
  }, hadeda_grpc_consensus_submit_message = function(config,
                                                      topic_id,
                                                      payload,
                                                      memo,
                                                      chunk_info,
                                                      wait_for_receipt) {
    calls[[length(calls) + 1]] <<- list(
      topic_id = topic_id,
      payload = payload,
      memo = memo,
      chunk_info = chunk_info,
      wait_for_receipt = wait_for_receipt
    )
    response
  })

  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$topic_id, "0.0.2000")
  expect_true(is.raw(calls[[1]]$payload))
  expect_null(calls[[1]]$chunk_info)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$chunk_number, 1)
  expect_equal(tbl$transaction_id, response$transaction_id)
  expect_equal(tbl$status, response$status)
  expect_equal(tbl$receipt_status, "SUCCESS")
  expect_true(tbl$acknowledged)
  expect_equal(tbl$sequence_number, 42)
  expect_s3_class(tbl$consensus_timestamp, "POSIXct")
})

test_that("consensus_submit_message chunks large payloads", {
  cfg <- hadeda_config(network = "testnet")
  cfg$default_transport <- "grpc"
  cfg$grpc$operator_account_id <- "0.0.5005"

  payload <- paste(rep("a", 8000), collapse = "")
  responses <- list(
    list(
      transaction_id = "0.0.5005-1700000000-000000000",
      status = "OK",
      receipt = list(status = "SUCCESS", sequenceNumber = 1),
      consensusTimestamp = "1700000000.000000001"
    ),
    list(
      transaction_id = "0.0.5005-1700000001-000000000",
      status = "OK",
      receipt = list(status = "SUCCESS", sequenceNumber = 2),
      consensusTimestamp = "1700000001.000000000"
    )
  )

  observed_chunks <- list()
  with_mocked_bindings({
    tbl <- consensus_submit_message(
      cfg,
      topic_id = "0.0.6006",
      message = payload,
      message_type = "text",
      chunk_size = 4096L
    )
  }, hadeda_generate_transaction_id = function(config) {
    "0.0.5005-1700000000-000000000"
  }, hadeda_grpc_consensus_submit_message = function(config,
                                                     topic_id,
                                                     payload,
                                                     memo,
                                                     chunk_info,
                                                     wait_for_receipt) {
    observed_chunks[[length(observed_chunks) + 1]] <<- list(
      topic_id = topic_id,
      payload = payload,
      chunk_info = chunk_info
    )
    responses[[length(observed_chunks)]]
  })

  expect_equal(length(observed_chunks), 2)
  expect_true(all(vapply(observed_chunks, function(call) is.raw(call$payload), logical(1))))
  expect_true(all(vapply(observed_chunks, function(call) {
    length(call$payload) <= 4096
  }, logical(1))))
  expect_true(all(vapply(seq_along(observed_chunks), function(i) {
    call <- observed_chunks[[i]]
    call$chunk_info$number == i &&
      call$chunk_info$total == length(observed_chunks) &&
      identical(call$chunk_info$initial_transaction_id, "0.0.5005-1700000000-000000000")
  }, logical(1))))

  expect_equal(nrow(tbl), 2)
  expect_equal(tbl$chunk_number, 1:2)
  expect_true(all(tbl$acknowledged))
  expect_equal(tbl$sequence_number, c(1, 2))
})
