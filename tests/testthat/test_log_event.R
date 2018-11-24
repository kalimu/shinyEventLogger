context("test_log_event")

test_that("logging events", {capture.output({

  expect_error(fixed = TRUE,
    log_event(),
    "Use set_logging() before logging events."
  )

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)

  expect_identical(
    log_event(),
    list(
      counter = 1,
      entry = "|#1|EVENT||FIRED|"
      )
    )

  expect_identical(
    log_event("Event body",
              name = "Event name",
              type = "TYPE",
              status = "DONE",
              params = list(a = 1, b = 2)
              ),
    list(
      counter = 2,
      entry = "|#2|TYPE|Event name|DONE|list(a = 1, b = 2)\n|#2|Event body\n"
      )
    )

  expect_message(fixed = TRUE,
    log_event("Test", "compound", "event"),
    "|#3|EVENT|Test compound event|FIRED|"
    )

  expect_error(fixed = TRUE,
    log_event(params = "a = 1"),
    "The 'params' argument should be a list, not a character"
    )

})}) # end of test_that

