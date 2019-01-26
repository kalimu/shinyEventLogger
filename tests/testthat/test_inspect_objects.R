context("inspect_objects")

test_that("inspect_objects", {

  expect_true(inspect_object())
  expect_true(inspect_objects())

  expect_message(fixed = TRUE,
    inspect_objects(object1 = mtcars),
"Object named `object1` was assigned to the global environment for debugging."
  )

  expect_true(exists('object1'))
  expect_identical(object1, mtcars)

  rm(object1, envir = .GlobalEnv)

  local(inspect_objects(object1 = mtcars))

  expect_true(exists('object1'))
  expect_identical(object1, mtcars)


}) # end of test_that

