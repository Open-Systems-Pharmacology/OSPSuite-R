test_that("It can load a valid MoBi project", {
  moBiProject <- loadMoBiProject(filePath = getTestDataFilePath("TH_QST_Platform.mbp3"))
  expect_true(isOfType(moBiProject, "MoBiProject"))
  expect_equal(moBiProject$sourceFile, normalizePath(getTestDataFilePath("TH_QST_Platform.mbp3"), winslash = "/"))
})

defaultMoBiProject <- loadMoBiProject(filePath = getTestDataFilePath("TH_QST_Platform.mbp3"))
emptyProject <- loadMoBiProject(filePath = getTestDataFilePath("Empty_Project.mbp3"))

test_that("It can print a MoBi project", {
  expect_snapshot(print(defaultMoBiProject))
})

# Test for MoBiProject$simulationNames
test_that("It can get simulation names from a MoBi project", {
  expectedNames <- c("Thyroid_QST_Human",
                     "Thyroid_QST_Phenobarbital")
  expect_equal(defaultMoBiProject$simulationNames, expectedNames)
  expect_equal(emptyProject$simulationNames, character(0))
  # Test that simulationNames is read-only
  expect_error(defaultMoBiProject$simulationNames <- "NewSimulation",
               "Property 'simulationNames' is read-only")
})

# Test for MoBiProject$parameterIdentificationNames 2DO
# test_that("It can get parameter identification names from a MoBi project", {
#   expectedNames <- c("Thyroid_QST_Human",
#                      "Thyroid_QST_Phenobarbital")
#   expect_equal(defaultMoBiProject$parameterIdentificationNames, expectedNames)
#   expect_equal(emptyProject$parameterIdentificationNames, character(0))
#   # Test that parameterIdentificationNames is read-only
#   expect_error(defaultMoBiProject$parameterIdentificationNames <- "NewParameterIdentification",
#                "Property 'parameterIdentificationNames' is read-only")
# })

# Test for MoBiProject$individualsNames
test_that("It can get individuals names from a MoBi project", {
  expectedNames <- c("Human",
                     "Rat")
  expect_equal(defaultMoBiProject$individualsNames, expectedNames)
  expect_equal(emptyProject$individualsNames, character(0))
  # Test that individualsNames is read-only
  expect_error(defaultMoBiProject$individualsNames <- "NewIndividual",
               "Property 'individualsNames' is read-only")
})

# Test for MoBiProject$expressionProfilesNames
test_that("It can get expression profiles names from a MoBi project", {
  expectedNames <- c("UDPGT1|Human|Healthy",
                     "DIO1|Human|Healthy",
                     "DIO3|Human|Healthy",
                     "UDPGT2|Human|Healthy",
                     "UGT1A1|Rat|Healthy",
                     "PB-LiverBindingPartner|Human|Healthy")
  expect_equal(defaultMoBiProject$expressionProfilesNames, expectedNames)
  expect_equal(emptyProject$expressionProfilesNames, character(0))
  # Test that expressionProfilesNames is read-only
  expect_error(defaultMoBiProject$expressionProfilesNames <- "NewExpressionProfile",
               "Property 'expressionProfilesNames' is read-only")
})

# Test for MoBiProject$getModules
test_that("It can get modules from a MoBi project", {
  expectedNames <- c("Thyroid_QST",
                     "TH_activeTransports",
                     "Pituitary",
                     "Phenobarbital_Extension",
                     "Phenobarbital_PBPK",
                     "Endogenous_TH",
                     "TH_plasma_binding",
                     "Thyroid",
                     "Rat physiology")

  modules <- defaultMoBiProject$getModules()
  expect_true(isOfType(modules, "MoBiModule"))
  expect_equal(names(modules), expectedNames)
  modules <- emptyProject$getModules()
  expect_equal(names(modules), character(0))
})

# Test for MoBiProject$getIndividual
test_that("It can get an individual from a MoBi project", {
  individual <- defaultMoBiProject$getIndividual("Human")
  expect_true(isOfType(individual, "BuildingBlock"))
  expect_true(individual$type == "Individual")
  expect_equal(individual$name, "Human")

  # Test for non-existing individual
  expect_error(defaultMoBiProject$getIndividual("NonExistingIndividual"),
               messages$errorIndividualNotFound("NonExistingIndividual"))
})

test_that("It correctly handles non-existing individuals", {
  # Test for non-existing individual with stopIfNotFound = FALSE
  individual <- defaultMoBiProject$getIndividual("NonExistingIndividual", stopIfNotFound = FALSE)
  expect_null(individual)
})
