context("log_to_file")

test_that("logging events", {

  expect_error(fixed = TRUE,
    shinyEventLogger:::log_to_file(),
    "A header of log entry is missing."
    )

  # expect_false(
  #   shinyEventLogger:::log_to_file()
  #   )
  #
  # shinyEventLogger:::log_to_file(header = )

})

