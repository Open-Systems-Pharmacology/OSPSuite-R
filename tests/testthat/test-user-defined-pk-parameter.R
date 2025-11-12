# UserDefinedPKParameter
test_that("It can print a user defined pk parameter", {
  userDefinedPKParameter <- addUserDefinedPKParameter(
    name = "MyAUC",
    standardPKParameter = StandardPKParameter$AUC_tEnd
  )
  userDefinedPKParameter$startTime <- 50
  userDefinedPKParameter$endTime <- 80
  expect_snapshot(print(userDefinedPKParameter))
})
