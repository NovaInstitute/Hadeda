test_that("hadeda_debug escapes cli braces", {
  original <- getOption("hadeda.debug")
  on.exit(options(hadeda.debug = original), add = TRUE)
  options(hadeda.debug = TRUE)

  expect_no_error(hadeda_debug("Seeking token '%s' (current token: %s)", "service", "{"))
  expect_no_error(hadeda_debug("Stopped at position %d while searching for '%s' (found: %s)", 10, "}", "<end>"))
})
