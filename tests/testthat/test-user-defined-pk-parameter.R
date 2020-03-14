context("UserDefinedPKParameter")


test_that("It print a user defined pk parameter", {
  userDefinedPKParameter <- addUserDefinedPKParameter("MyTmax", StandardPKParameter$Tmax)
  expect_error(capture.output(userDefinedPKParameter$print()), NA)
})
