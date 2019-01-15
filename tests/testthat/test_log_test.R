context("log_test")

test_that("logging wrapper functions", {capture.output({

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)

  set_logging_session()

  expect_message(fixed = TRUE,
    log_test(expect_true(FALSE)),
    "|#1|TEST|expect_true(FALSE)|ERROR|"
    )

  expect_output(fixed = TRUE,
    log_test(expect_true(FALSE)),
    "|#2|Error: FALSE isn't true."
    )

  expect_message(fixed = TRUE,
    log_test(expect_true(TRUE)),
    "|#3|TEST|expect_true(TRUE)|SUCCESS|"
    )

})})

