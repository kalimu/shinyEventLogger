context("log_to_r_console")

test_that("logging events", {capture.output({

  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_file(),
    "A header of log entry is missing."
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(header = "|#-1|EVENT||DONE|"),
    "|#-1|EVENT||DONE|"
    )

  expect_output(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(header = "|#-1|EVENT||DONE|",
                                        body = "|#-1|First line\n"),
    "|#-1|First line"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(header = "|#-1|EVENT||DONE|",
                                        body = "|#-1|First line\n"),
    "|#-1|EVENT||DONE|"
    )

})})

test_that("returned values", {capture.output({

  expect_identical(
    shinyEventLogger:::log_to_r_console(header = "|#-1|EVENT||DONE|"),
    "|#-1|EVENT||DONE|"
    )

  expect_identical(
    shinyEventLogger:::log_to_r_console(header = "|#-1|EVENT||DONE|",
                                        body = "|#-1|First line\n"),
    "|#-1|EVENT||DONE|\n|#-1|First line\n"
    )

})})

