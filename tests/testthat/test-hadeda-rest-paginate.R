test_that("hadeda_rest_paginate follows relative next links", {
  cfg <- hadeda_config(network = "testnet")

  responses <- list(
    list(links = list("next" = "accounts?cursor=abc")),
    list(links = list("next" = NULL))
  )

  calls <- list()
  idx <- 0

  with_mocked_bindings({
    result <- hadeda_rest_paginate(cfg, "accounts", list(limit = 1))
  }, hadeda_rest_get = function(config, path, query = list()) {
    idx <<- idx + 1
    calls[[idx]] <<- list(path = path, query = query)
    responses[[idx]]
  })

  expect_length(result, 2)
  expect_identical(calls[[1]]$path, "accounts")
  expect_identical(calls[[1]]$query$limit, 1)
  expect_identical(calls[[2]]$path, "accounts")
  expect_identical(calls[[2]]$query$cursor, "abc")
})

test_that("hadeda_rest_paginate trims base path prefixes", {
  cfg <- hadeda_config(network = "testnet")

  responses <- list(
    list(links = list("next" = "/api/v1/accounts?cursor=def")),
    list(links = list("next" = NULL))
  )

  calls <- list()
  idx <- 0

  with_mocked_bindings({
    hadeda_rest_paginate(cfg, "accounts", list())
  }, hadeda_rest_get = function(config, path, query = list()) {
    idx <<- idx + 1
    calls[[idx]] <<- list(path = path, query = query)
    responses[[idx]]
  })

  expect_identical(calls[[1]]$path, "accounts")
  expect_identical(calls[[2]]$path, "accounts")
  expect_identical(calls[[2]]$query$cursor, "def")
})

test_that("hadeda_rest_paginate preserves path when next link is query only", {
  cfg <- hadeda_config(network = "testnet")

  responses <- list(
    list(links = list("next" = "?cursor=ghi")),
    list(links = list("next" = NULL))
  )

  calls <- list()
  idx <- 0

  with_mocked_bindings({
    hadeda_rest_paginate(cfg, "transactions", list())
  }, hadeda_rest_get = function(config, path, query = list()) {
    idx <<- idx + 1
    calls[[idx]] <<- list(path = path, query = query)
    responses[[idx]]
  })

  expect_identical(calls[[1]]$path, "transactions")
  expect_identical(calls[[2]]$path, "transactions")
  expect_identical(calls[[2]]$query$cursor, "ghi")
})
