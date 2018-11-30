context("log_wrappers")

test_that("logging wrapper functions", {capture.output({

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)

  options("shinyEventLogger.counter" = 4)

  expect_message(fixed = TRUE,
    log_message("Message"),
    "|#4|MESSAGE|Message|FIRED|"
    )

  expect_warning(
    expect_message(fixed = TRUE,
      log_warning("Warning"),
      "|#5|WARNING|Warning|FIRED|"
    )
  )

  expect_error(
    expect_message(fixed = TRUE,
      log_error("Error"),
      "|#6|ERROR|Error|FIRED|"
    )
  )

})})

