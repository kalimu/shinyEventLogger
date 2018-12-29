context("log_to_database")

test_that("logging events", {

  expect_message(fixed = TRUE,
    set_logging(r_console = FALSE, js_console = FALSE, file = FALSE,
                database = TRUE),
    "File with URL to MongoDB doesn't exist."
    )

  expect_warning(fixed = TRUE,
    shinyEventLogger:::log_to_database(),
    "Use set_logging() to define log database."
    )

  expect_warning(
    expect_false(shinyEventLogger:::log_to_database())
  )

})

