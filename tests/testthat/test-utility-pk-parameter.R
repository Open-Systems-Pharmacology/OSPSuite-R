
context("addUserDefinedPKParameter")

test_that("It can add a user defined pk-parameter by name and type", {
  userDefinedPKParameter <- addUserDefinedPKParameter("MyTmax", StandardPKParameter$t_max)
  expect_equal(userDefinedPKParameter$name, "MyTmax")
  expect_equal(userDefinedPKParameter$displayName, "MyTmax")
  expect_equal(userDefinedPKParameter$dimension, "Time")
  expect_equal(userDefinedPKParameter$displayUnit, "h")
  removeAllUserDefinedPKParameters()
})

test_that("It can add a user defined pk-parameter with another display unit", {
  userDefinedPKParameter <- addUserDefinedPKParameter("MyTmax", StandardPKParameter$t_max, displayName = "MyTmaxDisplay", displayUnit = "min")
  expect_equal(userDefinedPKParameter$name, "MyTmax")
  expect_equal(userDefinedPKParameter$displayName, "MyTmaxDisplay")
  expect_equal(userDefinedPKParameter$dimension, "Time")
  expect_equal(userDefinedPKParameter$displayUnit, "min")
  removeAllUserDefinedPKParameters()
})

context("updatePKParameter")
test_that("It can update a pk parameter by name", {
  userDefinedPKParameter <- addUserDefinedPKParameter("MyTmax", StandardPKParameter$t_max, displayName = "MyTmaxDisplay", displayUnit = "min")
  updatePKParameter("MyTmax", displayName = "TOTO", displayUnit = "h")
  expect_equal(userDefinedPKParameter$displayName, "TOTO")
  expect_equal(userDefinedPKParameter$displayUnit, "h")
  removeAllUserDefinedPKParameters()
})


test_that("It throws an exception when updating a pkparameter with a unit that does not exist in the dimension", {
  userDefinedPKParameter <- addUserDefinedPKParameter("MyTmax", StandardPKParameter$t_max, displayUnit = "min")
  expect_that(updatePKParameter("MyTmax",  displayUnit = "mg"), throws_error())
  removeAllUserDefinedPKParameters()
})

context("removeAllUserDefinedPKParameters")

test_that("It can remove all user defined pk parameters", {
  addUserDefinedPKParameter("MyTmax", StandardPKParameter$t_max, displayName = "MyTmaxDisplay", displayUnit = "min")
  pkParam <- pkParameterByName("MyTmax")
  expect_false(is.null(pkParam))
  removeAllUserDefinedPKParameters()
  pkParam <- pkParameterByName("MyTmax", stopIfNotFound = FALSE)
  expect_null(pkParam)
})

context("pkParameterByName")

test_that("It should return null When returning a pk parameter by name that does not exist and no exception should be thrown", {
  pkParam <- pkParameterByName("MyTmax",  stopIfNotFound = FALSE)
  expect_null(pkParam)
})

test_that("It should throw an exception When returning a pk parameter by name that does not exist and an exception should be thrown", {
  expect_that(pkParameterByName("MyTmax",  stopIfNotFound = TRUE), throws_error())
})
