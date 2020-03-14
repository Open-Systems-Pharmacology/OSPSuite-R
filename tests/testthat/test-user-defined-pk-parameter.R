context("UserDefinedPKParameter")


test_that("It print a user defined pk parameter", {
  userDefinedPKParameter <- addUserDefinedPKParameter(name = "MyAUC", standardPKParameter = StandardPKParameter$AucTend)
  userDefinedPKParameter$startTime <- 50
  userDefinedPKParameter$endTime <- 80
  expect_error(capture.output(userDefinedPKParameter$print()), NA)
})
