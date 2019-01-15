context("log_event")

options(shinyEventLogger.file = NULL)
options(shinyEventLogger.database = NULL)
options(shinyEventLogger.r_console = NULL)
options(shinyEventLogger.js_console = NULL)

test_that("logging events", {capture.output({

  expect_error(fixed = TRUE,
    log_event(),
    "Use set_logging() before logging events."
  )

  set_logging(r_console  = TRUE,
              js_console = FALSE,
              file       = FALSE,
              database   = FALSE,
              global_1 = 1)


  expect_error(fixed = TRUE,
    log_event(),
    "'log_event_register' not found. Have you call 'set_logging_session'?"
  )

  set_logging_session(session_1 = 1)

  expect_identical(
    log_event(),
    list(
      counter = 1,
      entry =
        "|#1|EVENT||FIRED|\n|#1|PARAMS|list(session_1 = 1, global_1 = 1)\n"
      )
    )

  set_logging_session()

  expect_identical(
    log_event(),
    list(
      counter = 1,
      entry = "|#1|EVENT||FIRED|\n|#1|PARAMS|list(global_1 = 1)\n"
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
      entry = paste0("|#2|TYPE|Event name|DONE|\n",
                     "|#2|PARAMS|list(a = 1, b = 2, global_1 = 1)\n",
                     "|#2|Event body\n")
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

