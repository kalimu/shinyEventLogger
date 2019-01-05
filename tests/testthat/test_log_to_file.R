context("log_to_file")

test_that("missing params", {capture.output({

  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_file(),
    "A header of log entry is missing."
    )

  expect_warning(fixed = TRUE,
    shinyEventLogger:::log_to_file(header = "test"),
    "Use set_logging() to define log file."
    )

  options(shinyEventLogger.file = "nonexisting_file.log")
  expect_warning(fixed = TRUE,
    shinyEventLogger:::log_to_file(header = "test"),
    "File log 'nonexisting_file.log' does NOT exist!"
    )

})}) # end of test_that

test_that("logging to a file", {capture.output({

  temp_file <- tempfile()

  set_logging(r_console = FALSE, js_console = FALSE, file = temp_file)

  expect_true(file.exists(temp_file))

  expect_true(log_to_file(header = "test1", event_timestamp = Sys.time()))
  expect_true(log_to_file(header = "test2", event_timestamp = Sys.time()))

  eventlog <- strsplit(x = readLines(con = temp_file),
                       split = "|",
                       fixed = TRUE)

  expect_length(eventlog, 2)

  expect_identical(eventlog[[1]][1], "test1")
  expect_identical(eventlog[[2]][1], "test2")

  unlink(temp_file)

})}) # end of test_that
