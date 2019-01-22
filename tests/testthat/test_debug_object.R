context("debug_object")

test_that("debug_object", {

  expect_true(debug_object())
  expect_true(debug_objects())

  expect_message(fixed = TRUE,
    debug_object(object1 = mtcars),
"Object named `object1` was assigned to the global environment for debugging."
  )

  expect_true(exists('object1'))
  expect_identical(object1, mtcars)

  rm(object1, envir = .GlobalEnv)

  local(debug_object(object1 = mtcars))

  expect_true(exists('object1'))
  expect_identical(object1, mtcars)


}) # end of test_that

