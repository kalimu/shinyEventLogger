context("create_log_entry")

test_that("creating log entries", {

  expect_identical(
    shinyEventLogger:::create_log_entry(),
    list(
      header = "|#-1|EVENT||DONE|",
      body   = ""
    )
  )

  expect_identical(
    shinyEventLogger:::create_log_entry(
      event_counter = 23,
      event_type = "NEWTYPE",
      event_name = "Simple event",
      event_status = "STARTED",
      event_params = "list(a=1)"
    ),
    list(
      header = "|#23|NEWTYPE|Simple event|STARTED|list(a=1)",
      body = ""
    )
  )

  expect_identical(
    shinyEventLogger:::create_log_entry(
      event_counter = 23,
      event_type = "NEWTYPE",
      event_name = "Multiple-line event",
      event_status = "STARTED",
      event_params = "list(a=1)",
      event_body = "First line\nSecond line"
    ),
    list(
      header = "|#23|NEWTYPE|Multiple-line event|STARTED|list(a=1)",
      body = "|#23|First line\n|#23|Second line\n"
    )
  )

})

