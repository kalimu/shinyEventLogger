context("read_eventlog_file")

test_that("reading invalid eventlog file", {

  temp_file <- tempfile()

  set_logging(r_console = FALSE, js_console = FALSE, file = temp_file)

  expect_true(file.exists(temp_file))

  expect_true(log_to_file(header = "test1", event_timestamp = Sys.time()))
  expect_true(log_to_file(header = "test2", event_timestamp = Sys.time()))

  eventlog <- strsplit(x = readLines(con = temp_file),
                       split = "|",
                       fixed = TRUE)

  expect_error(fixed = TRUE,
    expect_message(fixed = TRUE,
      shinyEventLogger:::read_eventlog_file(file = temp_file),
      "Reading log file:"),
    "Some of the log records have unexpected length."
    )

  unlink(temp_file)

}) # end of test_that

test_that("reading valid eventlog file", {

  demo_filelog <- system.file("shiny", "demoapp/events.log",
                              package = "shinyEventLogger")

  expect_true(file.exists(demo_filelog))

  expect_message(fixed = TRUE,
                 set_logging(r_console = FALSE,
                             js_console = FALSE,
                             file = demo_filelog),
                 "Logging to the file:           ENABLED")

  eventlog <- shinyEventLogger:::read_eventlog_file(file = demo_filelog)

  expect_identical(
    names(eventlog),
    c("event_counter", "event_type", "event_name", "event_status",
      "session_id", "event_timestamp", "event_body", "event_id",
      "build", "logger_ver",
      "dataset", "fun", "resource",
      "n_rows", "bins", "variable")
    )

  expect_gt(NROW(eventlog), 0)

  expect_is(eventlog$event_timestamp, "POSIXct")
  expect_is(eventlog$event_timestamp, "POSIXt")
  expect_is(eventlog$build, "integer")

})

