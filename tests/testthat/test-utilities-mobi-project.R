test_that("It can load a valid MoBi project", {
  moBiProject <- loadMoBiProject(filePath = getTestDataFilePath("TH_QST_Platform.mbp3"))
  expect_true(isOfType(moBiProject, "MoBiProject"))
  expect_equal(moBiProject$sourceFile, normalizePath(getTestDataFilePath("TH_QST_Platform.mbp3"), winslash = "/"))
})

defaultMoBiProject <- loadMoBiProject(filePath = getTestDataFilePath("TH_QST_Platform.mbp3"))

# Test for MoBiProject$simulationNames
test_that("It can get simulation names from a MoBi project", {
  expectedNames <- c("Thyroid_QST_Human",
                     "Thyroid_QST_Phenobarbital")
  expect_equal(defaultMoBiProject$simulationNames, expectedNames)
  # Test that simulationNames is read-only
  expect_error(defaultMoBiProject$simulationNames <- "NewSimulation",
               "Property 'simulationNames' is read-only")
})

# Test for MoBiProject$parameterIdentificationNames 2DO
# test_that("It can get parameter identification names from a MoBi project", {
#   expectedNames <- c("Thyroid_QST_Human",
#                      "Thyroid_QST_Phenobarbital")
#   expect_equal(defaultMoBiProject$parameterIdentificationNames, expectedNames)
#   # Test that parameterIdentificationNames is read-only
#   expect_error(defaultMoBiProject$parameterIdentificationNames <- "NewParameterIdentification",
#                "Property 'parameterIdentificationNames' is read-only")
# })

# Test for MoBiProject$individualsNames
test_that("It can get individuals names from a MoBi project", {
  expectedNames <- c("Human",
                     "Rat")
  expect_equal(defaultMoBiProject$individualsNames, expectedNames)
  # Test that individualsNames is read-only
  expect_error(defaultMoBiProject$individualsNames <- "NewIndividual",
               "Property 'individualsNames' is read-only")
})
