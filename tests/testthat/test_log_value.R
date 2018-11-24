context("test_log_value")

test_that("logging multiline output events", {capture.output({

  set_logging(r_console = TRUE, js_console = FALSE, file = FALSE)

  env <- new.env()
  env$a = 23

  expect_message(fixed = TRUE,
    log_value(env$a),
    "|#1|VALUE|env$a|FIRED|"
    )

  expect_output(fixed = TRUE,
    log_value(env$a),
    "|#2|23"
    )

  expect_identical(
    log_value(env$a,
              type = "NEWTYPE",
              status = "NEWSTATUS",
              params = list(a = 1, b = 2)
              ),
    list(
     counter = 3,
     entry = "|#3|NEWTYPE|env$a|NEWSTATUS|list(a = 1, b = 2)\n|#3|23\n"
    ))

})})
