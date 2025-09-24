test_that("hadeda_config returns a functional configuration list", {
  cfg <- hadeda_config(network = "testnet")

  expect_equal(cfg$network, "testnet")
  expect_type(cfg$rest, "list")
  expect_type(cfg$grpc, "list")
  expect_true(cfg$default_transport %in% c("rest", "grpc"))
})

test_that("transport resolution respects overrides", {
  expect_equal(hadeda_resolve_transport(NULL, list(a = 1), list()), "rest")
  expect_equal(hadeda_resolve_transport(NULL, list(), list(a = 1)), "grpc")
  expect_error(hadeda_resolve_transport("invalid"), class = "hadeda_invalid_transport")
})

test_that("configuration overrides are merged shallowly", {
  defaults <- list(base_url = "https://example.com", headers = list(Accept = "json"))
  overrides <- list(headers = list(Accept = "xml"))
  merged <- hadeda_merge_config(defaults, overrides)

  expect_equal(merged$base_url, defaults$base_url)
  expect_equal(merged$headers$Accept, "xml")
})
