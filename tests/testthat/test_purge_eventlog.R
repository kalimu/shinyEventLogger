context("purge_eventlog")

test_that("reading invalid eventlog file", {

  demo_filelog <- system.file("shiny", "demoapp/events.log",
                              package = "shinyEventLogger")

  expect_true(file.exists(demo_filelog))

  temp_file <- tempfile()
  file_conn <- base::file(temp_file)
  writeLines(readLines(con = demo_filelog), file_conn)
  close(file_conn)

  eventlog <- shinyEventLogger::read_eventlog(file = temp_file)

  builds <- as.data.frame(table(eventlog$build), stringsAsFactors = FALSE)
  builds

  expect_error(fixed = TRUE,
    purge_eventlog(file = temp_file),
    "You need to specify purging criterium (eg. `min_build`)"
    )

  expect_message(fixed = TRUE,
    purge_eventlog(file = temp_file, min_build = max(as.integer(builds$Var1))),
    "Removing eventlog records..."
    )

  expect_message(fixed = TRUE,
    purge_eventlog(file = temp_file, min_build = max(as.integer(builds$Var1))),
    "Nothing to do. Build version found at the beginning of the eventlog file:"
    )

  expect_message(fixed = TRUE,
    expect_identical(
      NROW(shinyEventLogger::read_eventlog(file = temp_file)),
      builds[builds$Var1 == max(as.integer(builds$Var1)), "Freq"]
    ),
    "Reading log file"
  )

  unlink(temp_file)

}) # end of test_that

