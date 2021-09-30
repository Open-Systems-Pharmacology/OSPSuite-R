context("Validations")

test_that("it can validate that an integer type is an integer", {
  validateIsInteger(5)
  # This is just ot have an expectation. Validation throws if not ok
  expect_true(TRUE)
})


test_that("it can validate that an integer array type is an integer", {
  validateIsInteger(c(1, 5))
  # This is just to have an expectation. Validation throws if not ok
  expect_true(TRUE)
})

test_that("it throws a validation error when an object is not an integer", {
  expect_that(validateIsInteger(c(1.5, 5)), throws_error())
  expect_that(validateIsInteger(2.4), throws_error())
})

test_that("It accepts an empty string", {
  expect_error(validatePathIsAbsolute(""), NA)
})

test_that("It accepts a path without wildcard", {
  path <- "Organism|path"
  expect_error(validatePathIsAbsolute(path), NA)
})

test_that("It throws an error for a path with a wildcard", {
  path <- "Organism|*path"
  expect_error(validatePathIsAbsolute(path), messages$errorEntityPathNotAbsolute(path))
})

test_that("It does not throw an error when a number is indeed an integer", {
  validateIsOfType(object = 2, type = "integer")
  # This is just to have an expectation. Validation throws if not ok
  expect_true(TRUE)
})

test_that("It does throw an error when a number is not an integer", {
  expect_that(validateIsOfType(object = 2.5, type = "integer"), throws_error())
})
