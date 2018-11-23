context("log_to_file")

test_that("missing params", {

  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_file(),
    "A header of log entry is missing."
    )

  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_file(header = "test"),
    "Use set_logging() to define log file."
    )

  options(shinyEventLogger.file = "nonexisting_file.log")
  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_file(header = "test"),
    "File log 'nonexisting_file.log' does NOT exist!"
    )

}) # end of test_that

test_that("logging events", {

  set_logging(r_console = FALSE, js_console = FALSE, file = TRUE)

  expect_identical(
    getOption("shinyEventLogger.file"),
    "events.log"
    )

  #  TODO: shinyEventLogger:::log_to_file(header = "test")








}) # end of test_that