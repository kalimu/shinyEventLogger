context("log_wrappers")

test_that("logging wrapper functions", {capture.output({

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)
  set_logging_session()

  expect_message(fixed = TRUE,
    log_message("Message"),
    "|#1|MESSAGE|Message|FIRED|"
    )

  expect_warning(
    expect_message(fixed = TRUE,
      log_warning("Warning"),
      "|#2|WARNING|Warning|FIRED|"
    )
  )

  expect_error(
    expect_message(fixed = TRUE,
      log_error("Error"),
      "|#3|ERROR|Error|FIRED|"
    )
  )

})})

