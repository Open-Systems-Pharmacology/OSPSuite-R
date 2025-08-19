defaultMoBiProject <- loadMoBiProject(filePath = getTestDataFilePath("TH_QST_Platform.mbp3"))
emptyProject <- loadMoBiProject(filePath = getTestDataFilePath("Empty_Project.mbp3"))

test_that("It can print a MoBi project", {
  expect_snapshot(print(defaultMoBiProject))
})

# Test for MoBiProject$simulationNames
test_that("It can get simulation names from a MoBi project", {
  expectedNames <- c(
    "Thyroid_QST_Human",
    "Thyroid_QST_Phenobarbital"
  )
  expect_equal(defaultMoBiProject$simulationNames, expectedNames)
  expect_equal(emptyProject$simulationNames, character(0))
  # Test that simulationNames is read-only
  expect_error(
    defaultMoBiProject$simulationNames <- "NewSimulation",
    "Property 'simulationNames' is read-only"
  )
})

test_that("It can get parameter identification names from a MoBi project", {
  expectedNames <- "Parameter Identification 1"
  expect_equal(defaultMoBiProject$parameterIdentificationNames, expectedNames)
  expect_equal(emptyProject$parameterIdentificationNames, character(0))
  # Test that parameterIdentificationNames is read-only
  expect_error(
    defaultMoBiProject$parameterIdentificationNames <- "NewParameterIdentification",
    "Property 'parameterIdentificationNames' is read-only"
  )
})

# Test for MoBiProject$individualNames
test_that("It can get individuals names from a MoBi project", {
  expectedNames <- c(
    "Human",
    "Rat"
  )
  expect_equal(defaultMoBiProject$individualNames, expectedNames)
  expect_equal(emptyProject$individualNames, character(0))
  # Test that individualNames is read-only
  expect_error(
    defaultMoBiProject$individualNames <- "NewIndividual",
    "Property 'individualNames' is read-only"
  )
})

# Test for MoBiProject$expressionProfilesNames
test_that("It can get expression profiles names from a MoBi project", {
  expectedNames <- c(
    "UDPGT1|Human|Healthy",
    "DIO1|Human|Healthy",
    "DIO3|Human|Healthy",
    "UDPGT2|Human|Healthy",
    "UGT1A1|Rat|Healthy",
    "PB-LiverBindingPartner|Human|Healthy"
  )
  expect_equal(defaultMoBiProject$expressionProfilesNames, expectedNames)
  expect_equal(emptyProject$expressionProfilesNames, character(0))
  # Test that expressionProfilesNames is read-only
  expect_error(
    defaultMoBiProject$expressionProfilesNames <- "NewExpressionProfile",
    "Property 'expressionProfilesNames' is read-only"
  )
})

# Test for MoBiProject$getModules
test_that("It can get all modules from a MoBi project", {
  expectedNames <- c(
    "Thyroid_QST",
    "TH_activeTransports",
    "Pituitary",
    "Phenobarbital_Extension",
    "Phenobarbital_PBPK",
    "Endogenous_TH",
    "TH_plasma_binding",
    "Thyroid",
    "Rat physiology"
  )

  modules <- defaultMoBiProject$getModules()
  expect_true(isOfType(modules, "MoBiModule"))
  expect_equal(names(modules), expectedNames)
  modules <- emptyProject$getModules()
  expect_equal(names(modules), character(0))
})

test_that("It can get a specific module from a MoBi project", {
  module <- defaultMoBiProject$getModules("Thyroid_QST")
  expect_true(isOfType(module, "MoBiModule"))
  expect_equal(names(module), "Thyroid_QST")

  # Test for non-existing module
  expect_error(
    defaultMoBiProject$getModules("NonExistingModule"),
    messages$modulesNotPresentInProject("NonExistingModule"),
    fixed = TRUE
  )
})

# Test for MoBiProject$getIndividual
test_that("It can get an individual from a MoBi project", {
  individual <- defaultMoBiProject$getIndividual("Human")
  expect_true(isOfType(individual, "BuildingBlock"))
  expect_true(individual$type == BuildingBlockTypes$Individual)
  expect_equal(individual$name, "Human")

  # Test for non-existing individual
  expect_error(
    defaultMoBiProject$getIndividual("NonExistingIndividual"),
    messages$errorIndividualNotFound("NonExistingIndividual")
  )
})

test_that("It correctly handles non-existing individuals", {
  # Test for non-existing individual with stopIfNotFound = FALSE
  individual <- defaultMoBiProject$getIndividual("NonExistingIndividual", stopIfNotFound = FALSE)
  expect_null(individual)
})

# Test for MoBiProject$getSimulation
test_that("It returns a simulation object for getSimulation with existing simulation", {
  simulation <- defaultMoBiProject$getSimulation("Thyroid_QST_Human")
  expect_true(isOfType(simulation, "Simulation"))
  expect_equal(simulation$name, "Thyroid_QST_Human")

  # Test for non-existing simulation
  expect_error(
    defaultMoBiProject$getSimulation("NonExistingSimulation"),
    messages$errorSimulationNotFound("NonExistingSimulation"),
    fixed = TRUE
  )
})

test_that("It correctly handles non-existing simulations", {
  # Test for non-existing simulation with stopIfNotFound = FALSE
  simulation <- defaultMoBiProject$getSimulation("NonExistingSimulation", stopIfNotFound = FALSE)
  expect_null(simulation)
})

# Test for MoBiProject$getExpressionProfiles
test_that("It can get expression profiles from a MoBi project", {
  expressionProfiles <- defaultMoBiProject$getExpressionProfiles(c("UDPGT1|Human|Healthy", "DIO1|Human|Healthy"))
  expect_true(isOfType(expressionProfiles, "BuildingBlock"))
  expect_true(expressionProfiles[[1]]$type == BuildingBlockTypes$`Expression Profile`)
  expect_equal(names(expressionProfiles), c("UDPGT1|Human|Healthy", "DIO1|Human|Healthy"))

  # Test for non-existing expression profile
  expect_error(defaultMoBiProject$getExpressionProfiles(names = "NonExistingProfile"),
    messages$errorExpressionProfileNotFound(c("NonExistingProfile")),
    fixed = TRUE
  )

  # Test for a list of expression profiles where one profile is non-existent
  expect_error(
    defaultMoBiProject$getExpressionProfiles(names = c(
      "NonExistingProfile",
      "DIO1|Human|Healthy",
      "nonExistingTheSecond"
    )),
    messages$errorExpressionProfileNotFound(names = c("NonExistingProfile", "nonExistingTheSecond")),
    fixed = TRUE
  )
})

test_that("It correctly handles non-existing expression profiles", {
  # Test for non-existing expression profile with stopIfNotFound = FALSE
  expressionProfiles <- defaultMoBiProject$getExpressionProfiles(c("NonExistingProfile"), stopIfNotFound = FALSE)
  expect_vector(expressionProfiles, list(), size = 0)

  expressionProfiles <- defaultMoBiProject$getExpressionProfiles(
    names = c(
      "NonExistingProfile",
      "DIO1|Human|Healthy",
      "nonExistingTheSecond"
    ),
    stopIfNotFound = FALSE
  )
  expect_equal(names(expressionProfiles), "DIO1|Human|Healthy")
})

# Test for MoBiProject$getDataSets
test_that("It can get all data sets from a MoBi project", {
  allDataSetNames <- c(
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_91 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_179 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.6",
    "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.6",
    "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_91 mg/kg/day_po_Fig.6",
    "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.6",
    "Liu 1995_Total TSH__Rat__VenousBlood_Plasma_179 mg/kg/day_po_Fig.6",
    "Liu 1995_Total T4__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.2",
    "Liu 1995_Total T4__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.2",
    "Liu 1995_Total T4__Rat__VenousBlood_Plasma_91 mg/kg/day_po_Fig.2",
    "Liu 1995_Total T4__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.2",
    "Liu 1995_Total T4__Rat__VenousBlood_Plasma_179 mg/kg/day_po_Fig.2"
  )
  dataSets <- defaultMoBiProject$getObservedData()
  expect_true(isOfType(dataSets, "DataSet"))
  expect_equal(names(dataSets), allDataSetNames)

  # Test for empty project
  emptyDataSets <- emptyProject$getObservedData()
  expect_length(emptyDataSets, 0)
})

test_that("It can get specific data sets from a MoBi project", {
  dataSetNames <- c(
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_46 mg/kg/day_po_Fig.5A"
  )
  dataSets <- defaultMoBiProject$getObservedData(dataSetNames)
  expect_true(isOfType(dataSets, "DataSet"))
  expect_equal(names(dataSets), dataSetNames)

  # Test for non-existing data set
  expect_error(
    defaultMoBiProject$getObservedData("NonExistingDataSet"),
    messages$errorDataSetsNotPresentInProject(c("NonExistingDataSet")),
    fixed = TRUE
  )

  # Test for a list of data sets where one is non-existing
  expect_error(
    defaultMoBiProject$getObservedData(c("NonExistingDataSet", "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A")),
    messages$errorDataSetsNotPresentInProject(c("NonExistingDataSet")),
    fixed = TRUE
  )
})

test_that("It correctly handles non-existing data sets", {
  # Test for non-existing data set with stopIfNotFound = FALSE
  dataSets <- defaultMoBiProject$getObservedData("NonExistingDataSet", stopIfNotFound = FALSE)
  expect_length(dataSets, 0)

  # Test for a list of data sets where one is non-existing
  dataSets <- defaultMoBiProject$getObservedData(
    c("NonExistingDataSet", "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A"),
    stopIfNotFound = FALSE
  )
  expect_equal(names(dataSets), "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A")
})

# Test for MoBiProject$createSimulationConfiguration
# TODO https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1597
# test_that("It can create a simulation configuration from a MoBi project", {
#   modulesNames <- "Rat physiology"
#
#   simConfig <- defaultMoBiProject$createSimulationConfiguration(modulesNames = modulesNames)
# })


# test modules with no IC BB
# test modules with no PV BB
