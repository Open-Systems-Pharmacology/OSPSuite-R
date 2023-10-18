# UserDefinedPKParameter


test_that("It print a user defined pk parameter", {
  skip_on_os("linux") # TODO enable again as soon as https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/435 was fixed

  userDefinedPKParameter <- addUserDefinedPKParameter(name = "MyAUC", standardPKParameter = StandardPKParameter$AUC_tEnd)
  userDefinedPKParameter$startTime <- 50
  userDefinedPKParameter$endTime <- 80
  expect_error(capture.output(userDefinedPKParameter$print()), NA)
})
