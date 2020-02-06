context("Validations")

test_that("it can validate that an integer type is an integer", {
  validateIsInteger(5)
  #This is just ot have an expectation. Validation throws if not ok
  expect_true(TRUE)
})


test_that("it can validate that an integer array type is an integer", {
  validateIsInteger(c(1, 5))
  #This is just ot have an expectation. Validation throws if not ok
  expect_true(TRUE)
})

test_that("it throws a vlaidatin error when an object is not an integer", {
  expect_that(validateIsInteger(c(1.5, 5)), throws_error() )
  expect_that(validateIsInteger(2.4), throws_error() )
})
