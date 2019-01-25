context("log_output")

test_that("logging multiline output events", {capture.output({

  set_logging(r_console = TRUE,
              js_console = FALSE,
              file = FALSE,
              database = FALSE)

  set_logging_session()

  expect_identical(
    log_output(str(mtcars[1:2])),
    list(
      counter = 1,
      entry = paste0(
        "|#1|OUTPUT|str(mtcars[1:2])|FIRED|\n",
        "|#1|'data.frame':\t32 obs. of  2 variables:\n",
        "|#1| $ mpg: num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...\n",
        "|#1| $ cyl: num  6 6 4 6 8 6 8 4 4 6 ...\n"
        )
      )
    ) # end of expect_identical

  expect_message(fixed = TRUE,
    log_output(str(mtcars[1:2])),
    "|#2|OUTPUT|str(mtcars[1:2])|FIRED|"
    )

  expect_output(fixed = TRUE,
    log_output(str(mtcars[1:2])),
    "|#3|'data.frame':\t32 obs. of  2 variables:\n",
    "|#3| $ mpg: num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...\n",
    "|#3| $ cyl: num  6 6 4 6 8 6 8 4 4 6 ...\n"
    )

})})

test_that("outputs of expressions outside current env", {capture.output({

  set_logging(r_console = TRUE,
              js_console = FALSE,
              file = FALSE,
              database = FALSE)

  set_logging_session()

  env <- new.env()
  env$fun <- function(a) {print(a)}

  expect_message(fixed = TRUE,
    log_output(env$fun("test")),
    '|#1|OUTPUT|env$fun("test")|FIRED|'
    )

  expect_output(fixed = TRUE,
    log_output(env$fun("test")),
    '|#2|[1] "test"'
    )

})})

test_that("logging output with different params", {capture.output({

  set_logging(r_console = TRUE,
              js_console = FALSE,
              file = FALSE,
              database = FALSE)

  set_logging_session()

  expect_message(fixed = TRUE,
    log_output(str(mtcars[1:2]),
               type = "NEWTYPE",
               status = "NEWSTATUS",
               params = list(a = 1, b = 2)
               ),
    "|#1|NEWTYPE|str(mtcars[1:2])|NEWSTATUS|"
    )

  expect_output(fixed = TRUE,
    log_output(str(mtcars[1:2]),
               type = "NEWTYPE",
               status = "NEWSTATUS",
               params = list(a = 1, b = 2)
               ),
    "|#2|PARAMS|list(a = 1, b = 2)",
    "|#2|'data.frame':\t32 obs. of  2 variables:\n",
    "|#2| $ mpg: num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...\n",
    "|#2| $ cyl: num  6 6 4 6 8 6 8 4 4 6 ...\n"
    )

})})

