context("test_log_wrappers")

test_that("logging wrapper functions", {capture.output({

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)

  options("shinyEventLogger.counter" = 2)

  expect_message(fixed = TRUE,
    log_test(expect_true(FALSE)),
    "|#2|TEST|expect_true(FALSE)|FIRED|"
    )

  expect_output(
    log_test(expect_true(FALSE)),
    "|#3|Error: FALSE isn't true."
    )

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

