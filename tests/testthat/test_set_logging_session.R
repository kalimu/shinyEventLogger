context("set_logging_session")

test_that("no params", {

  expect_true(set_logging_session())

  expect_true(exists("log_event_register"))
  expect_true(exists("log_settings_session"))

  expect_true(is.environment(log_event_register))
  expect_true(is.environment(log_settings_session))

}) # end of test_that

test_that("with session params", {

  expect_true(set_logging_session(param1 = 1, param2 = "b"))

  expect_true(exists("log_event_register"))
  expect_true(exists("log_settings_session"))

  expect_true(is.environment(log_event_register))
  expect_true(is.environment(log_settings_session))

  expect_identical(log_settings_session$param1, 1)
  expect_identical(log_settings_session$param2, "b")


}) # end of test_that
