context("set_logging")


test_that("no settings", {

  expect_null(getOption("shinyEventLogger.file"))
  expect_null(getOption("shinyEventLogger.r_console"))
  expect_null(getOption("shinyEventLogger.js_console"))
  expect_null(getOption("shinyEventLogger.counter"))

})

test_that("default settings", {

  expect_message(
    fixed = TRUE,
    set_logging(),
    "Logging to R console:          ENABLED"
    )

  expect_message(
    fixed = TRUE,
    set_logging(),
    "Logging to JavaScript console: ENABLED"
    )

  expect_message(
    fixed = TRUE,
    set_logging(),
    "Logging to a file:             DISABLED"
    )

  expect_true(getOption("shinyEventLogger.r_console"))
  expect_true(getOption("shinyEventLogger.js_console"))

  expect_false(getOption("shinyEventLogger.file"))

  expect_identical(getOption("shinyEventLogger.counter"), 1)

})











# set_logging <- function(r_console = TRUE,
#                         js_console = TRUE,
#                         file = FALSE) {