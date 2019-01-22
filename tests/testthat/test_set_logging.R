context("set_logging")

options(shinyEventLogger.file = NULL)
options(shinyEventLogger.database = NULL)
options(shinyEventLogger.r_console = NULL)
options(shinyEventLogger.js_console = NULL)

test_that("no settings", {

  expect_null(getOption("shinyEventLogger.file"))
  expect_null(getOption("shinyEventLogger.database"))
  expect_null(getOption("shinyEventLogger.r_console"))
  expect_null(getOption("shinyEventLogger.js_console"))

  expect_message(fixed = TRUE,
    set_logging(r_console = FALSE, js_console = FALSE,
                file = FALSE, database = FALSE),
    "All types of logging are disabled!"
  )

  expect_identical(
    set_logging(r_console = FALSE, js_console = FALSE,
                file = FALSE, database = FALSE),
    FALSE
  )

}) # end of test_that

test_that("default settings", {capture.output({

  expect_message(fixed = TRUE,
    set_logging(),
    "Logging to R console:          ENABLED"
    )

  expect_message(fixed = TRUE,
    set_logging(),
    "Logging to JavaScript console: ENABLED"
    )

  expect_message(fixed = TRUE,
    set_logging(),
    "Logging to a file:             disabled"
    )

  expect_message(fixed = TRUE,
    set_logging(),
    "Logging to a database:         disabled"
    )

  set_logging()

  expect_true(getOption("shinyEventLogger.r_console"))
  expect_true(getOption("shinyEventLogger.js_console"))

  expect_false(getOption("shinyEventLogger.file"))
  expect_false(getOption("shinyEventLogger.database"))

})}) # end of test_that

test_that("creating log file", {capture.output({


  set_logging(r_console = FALSE,
              js_console = FALSE,
              file = TRUE,
              database = FALSE)

  expect_identical(
    getOption("shinyEventLogger.file"),
    "events.log"
    )

  if (file.exists("events.log")) unlink("events.log")
  expect_message(fixed = TRUE,
    set_logging(r_console = FALSE, js_console = FALSE, file = TRUE),
    "File log doesn't exist."
  )

  if (file.exists("events.log")) unlink("events.log")
  expect_message(fixed = TRUE,
    set_logging(r_console = FALSE, js_console = FALSE, file = TRUE),
    "Your current working directory:"
  )

  if (file.exists("events.log")) unlink("events.log")
  expect_message(fixed = TRUE,
    set_logging(r_console = FALSE, js_console = FALSE, file = TRUE),
    "New log file: 'events.log' has been created."
  )

  unlink("events.log")

  if (file.exists("custom_file.log")) unlink("custom_file.log")

  expect_message(fixed = TRUE,
    set_logging(r_console = FALSE,
                js_console = FALSE,
                file = "custom_file.log"),
    "New log file: 'custom_file.log' has been created."
  )

  unlink("custom_file.log")

  temp_file <- tempfile()

  set_logging(r_console = FALSE,
              js_console = FALSE,
              file = temp_file)

  expect_true(file.exists(temp_file))

  unlink(temp_file)

})}) # end of test_that

