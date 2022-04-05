
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

test_that("It calculates the pk parameters in the expected units", {
  sim <- loadTestSimulation("S1")
  c_max_base <- updatePKParameter(name = "C_max", displayUnit = "µmol/l")
  myCmax <- addUserDefinedPKParameter(name = "MyCMax", standardPKParameter = StandardPKParameter$C_max, displayUnit = "mg/l")
  quantityPath <- "Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)"
  mw <- sim$molWeightFor(quantityPath)
  results <- runSimulation(sim)
  pkAnalyses <- calculatePKAnalyses(results)
  c_max_base_value <- pkAnalyses$pKParameterFor(quantityPath, c_max_base$name)$values[1]
  c_max_mg_l_value <- c_max_base_value * mw * 1E6
  df <- pkAnalysesToDataFrame(pkAnalyses)
  df2 <- pkAnalysesToTibble(pkAnalyses)
  df_c_max <- df[df$Parameter == myCmax$name & df$QuantityPath == quantityPath, ]
  expect_equal(c_max_mg_l_value, df_c_max$Value)
  expect_s3_class(df, "data.frame")
  expect_s3_class(df, "tbl_df")
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
  pkParam <- pkParameterByName("MyTmax", stopIfNotFound = FALSE)
  expect_null(pkParam)
})

test_that("It should throw an exception When returning a pk parameter by name that does not exist and an exception should be thrown", {
  expect_that(pkParameterByName("MyTmax", stopIfNotFound = TRUE), throws_error())
})
