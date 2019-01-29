context("event_counter")

test_that("get_event_counter", {

  expect_error(fixed = TRUE,
    get_event_counter(),
    "'log_settings_session' not found. Have you call 'set_logging_session'?"
  )

  set_logging_session()

  expect_identical(get_event_counter(), 1)

}) # end of test_that

test_that("increment_event_counter", {

  suppressWarnings({
    rm(log_event_register)
    rm(log_settings_session)
  })

  set_logging_session()

  expect_identical(get_event_counter(), 1)

  increment_event_counter()

  expect_identical(get_event_counter(), 2)

}) # end of test_that
