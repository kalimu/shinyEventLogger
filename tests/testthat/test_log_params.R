context("log_params")

test_that("no settings", {

  log_params(param1 = "param1_value")

  expect_true(is.environment(log_settings))

  expect_identical(ls(envir = log_settings), "param1")

  expect_identical(
    log_settings$param1,
    "param1_value"
    )

  rm(log_settings)

}) # end of test_that

