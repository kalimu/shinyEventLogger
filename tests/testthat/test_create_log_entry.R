context("create_log_entry")

test_that("creating log entries", {capture.output({

  expect_identical(
    shinyEventLogger:::create_log_entry(
      event_counter = 23,
      event_type = "EVENT",
      event_name = "Simple event",
      event_status = "DONE",
      event_params = "list(a=1)",
      event_to_log = ""
    ),
    list(
      header = "|#23|EVENT|Simple event|DONE|list(a=1)",
      body = ""
    )
  )

  expect_identical(
    shinyEventLogger:::create_log_entry(
      event_counter = 23,
      event_type = "EVENT",
      event_name = "Multiple-line event",
      event_status = "DONE",
      event_params = "list(a=1)",
      event_to_log = "First line\nSecond line"
    ),
    list(
      header = "|#23|EVENT|Multiple-line event|DONE|list(a=1)",
      body = "|#23|First line\n|#23|Second line\n"
    )
  )

})})

