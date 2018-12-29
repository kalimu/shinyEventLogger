context("log_started_done")

test_that("logging started and done", {capture.output({

  expect_identical(
    shinyEventLogger:::create_event_id("test1", "test2", name = "test_name"),
    "test_name"
  )

  expect_identical(
    shinyEventLogger:::create_event_id("test1", "test2", name = NULL),
    "test1 test2"
  )

})}) # end of test_that

test_that("logging started and done", {capture.output({

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)

  expect_error(fixed = TRUE,
    log_started(),
    paste0("Event must have some kind of unique 'name'",
           "or body passed to the '...' argument.")
    )

  expect_message(fixed = TRUE,
    log_started("test_event_1"),
    "|#1|EVENT|test_event_1|STARTED|"
    )

  expect_message(fixed = TRUE,
    log_started("test_event_2"),
    "|#2|EVENT|test_event_2|STARTED|"
    )

  expect_message(fixed = TRUE,
    log_started("test_event_3"),
    "|#3|EVENT|test_event_3|STARTED|"
    )

  expect_message(fixed = TRUE,
    log_done("test_event_3"),
    "|#3|EVENT|test_event_3|DONE|"
    )

  expect_message(fixed = TRUE,
    log_done("test_event_2"),
    "|#2|EVENT|test_event_2|DONE|"
    )

  expect_message(fixed = TRUE,
    log_done("test_event_1"),
    "|#1|EVENT|test_event_1|DONE|"
    )

})})

