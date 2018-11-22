context("log_to_r_console")

test_that("logging events", {capture.output({

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(),
    "|#-1|EVENT||DONE|"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console("Test simple event"),
    "|#-1|EVENT|Test simple event|DONE|"
    )


  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console("Simple event with name",
                                        event_name = "Event name"),
    "|#-1|EVENT|Event name|DONE|"
    )


  expect_output(fixed = TRUE,
    shinyEventLogger:::log_to_r_console("Simple event with name",
                                        event_name = "Event name"),
    "|#-1|Simple event with name"
    )

  expect_output(fixed = TRUE,
    shinyEventLogger:::log_to_r_console("Multiline event\nwith name",
                                        event_name = "Event name",
                                        event_counter = 23),
    "|#23|Multiline event\n|#23|with name"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console("Test", "compound", "event"),
    "|#-1|EVENT|Test compound event|DONE|"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(event_counter = 23),
    "|#23|EVENT||DONE|"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(event_type = "NEWTYPE"),
    "|#-1|NEWTYPE||DONE|"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(status = "STARTED"),
    "|#-1|EVENT||STARTED|"
    )

  expect_message(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(params = list(a = 1, b = 2, c = 3)),
    "|#-1|EVENT||DONE|list(a = 1, b = 2, c = 3)"
    )

  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_r_console(params = "a = 1"),
    "The 'params' argument should be a list, not a character"
    )

})})

test_that("returned values", {capture.output({

  expect_identical(
    shinyEventLogger:::log_to_r_console("Multiline event\nwith name",
                                        event_name = "Event name"),
    "|#-1|EVENT|Event name|DONE|\n|#-1|Multiline event\n|#-1|with name\n"
    )

  expect_identical(
    shinyEventLogger:::log_to_r_console("Simple event"),
    "|#-1|EVENT|Simple event|DONE|"
    )


})})

