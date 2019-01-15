context("read_eventlog")

test_that("reading valid eventlog file", {capture.output({

  demo_filelog <- system.file("shiny", "demoapp/events.log",
                              package = "shinyEventLogger")

  expect_true(file.exists(demo_filelog))

  expect_message(fixed = TRUE,
                 set_logging(r_console = FALSE,
                             js_console = FALSE,
                             file = demo_filelog),
                 "Logging to the file:           ENABLED")

  eventlog <- shinyEventLogger::read_eventlog(file = demo_filelog)

  expect_identical(
    names(eventlog),
    c("event_counter", "event_type", "event_name", "event_status",
      "session_id", "event_timestamp", "event_body", "event_id",
      "this_session", "build",
      "dataset", "fun", "resource",
      "secs",
      "n_rows", "bins", "variable",
      ".order")
    )

  expect_gt(NROW(eventlog), 0)

  expect_is(eventlog$event_timestamp, "POSIXct")
  expect_is(eventlog$event_timestamp, "POSIXt")
  expect_is(eventlog$build, "integer")

  expect_is(eventlog, "eventlog")
  expect_is(eventlog, "tbl_df")

})})

