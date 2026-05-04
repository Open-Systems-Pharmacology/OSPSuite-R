emptyProject <- loadMoBiProject(
  filePath = getTestDataFilePath("MoBiProject/Empty_Project.mbp3")
)

test_that("It can print a MoBi project", {
  expect_snapshot(print(globalTestMoBiProject))
})

# Test for MoBiProject$simulationNames
test_that("It can get simulation names from a MoBi project", {
  expectedNames <- c(
    "TestSim_2Modules",
    "TestSim_2Modules_noICSelected"
  )
  expect_equal(globalTestMoBiProject$simulationNames, expectedNames)
  expect_equal(emptyProject$simulationNames, character(0))
  # Test that simulationNames is read-only
  expect_error(
    globalTestMoBiProject$simulationNames <- "NewSimulation",
    "Property 'simulationNames' is read-only"
  )
})

test_that("It can get parameter identification names from a MoBi project", {
  expectedNames <- "Parameter Identification 1"
  expect_equal(
    globalTestMoBiProject$parameterIdentificationNames,
    expectedNames
  )
  expect_equal(emptyProject$parameterIdentificationNames, character(0))
  # Test that parameterIdentificationNames is read-only
  expect_error(
    globalTestMoBiProject$parameterIdentificationNames <- "NewParameterIdentification",
    "Property 'parameterIdentificationNames' is read-only"
  )
})

# Test for MoBiProject$individualNames
test_that("It can get individuals names from a MoBi project", {
  expectedNames <- c(
    "DefaultIndividual"
  )
  expect_equal(globalTestMoBiProject$individualNames, expectedNames)
  expect_equal(emptyProject$individualNames, character(0))
  # Test that individualNames is read-only
  expect_error(
    globalTestMoBiProject$individualNames <- "NewIndividual",
    "Property 'individualNames' is read-only"
  )
})

# Test for MoBiProject$expressionProfilesNames
test_that("It can get expression profiles names from a MoBi project", {
  expectedNames <- c(
    "UGT2B7|Human|Healthy",
    "CYP3A4|Human|Healthy"
  )
  expect_equal(globalTestMoBiProject$expressionProfilesNames, expectedNames)
  expect_equal(emptyProject$expressionProfilesNames, character(0))
  # Test that expressionProfilesNames is read-only
  expect_error(
    globalTestMoBiProject$expressionProfilesNames <- "NewExpressionProfile",
    "Property 'expressionProfilesNames' is read-only"
  )
})

# Test for MoBiProject$defaultSimulationSettings
test_that("It can get default simulation settings from a MoBi project", {
  simSettings <- globalTestMoBiProject$defaultSimulationSettings
  expect_true(isOfType(simSettings, "SimulationSettings"))
})

test_that("defaultSimulationSettings is read-only", {
  expect_error(
    globalTestMoBiProject$defaultSimulationSettings <- "newValue",
    "Property 'defaultSimulationSettings' is read-only"
  )
})

# Test for MoBiProject$getModules
test_that("It can get all modules from a MoBi project", {
  expectedNames <- c(
    "ExtModule_3IC_3PV",
    "ExtModule_noIC_noPV",
    "TestModule"
  )

  modules <- globalTestMoBiProject$getModules()
  expect_true(isOfType(modules, "MoBiModule"))
  expect_named(modules, expectedNames, ignore.order = TRUE)
  modules <- emptyProject$getModules()
  expect_equal(names(modules), character(0))
})

test_that("It can get a specific module from a MoBi project", {
  module <- globalTestMoBiProject$getModules("ExtModule_3IC_3PV")
  expect_true(isOfType(module, "MoBiModule"))
  expect_equal(names(module), "ExtModule_3IC_3PV")

  # Test for non-existing module
  expect_error(
    globalTestMoBiProject$getModules("NonExistingModule"),
    messages$modulesNotPresentInProject("NonExistingModule"),
    fixed = TRUE
  )
})

# Test for MoBiProject$getIndividual
test_that("It can get an individual from a MoBi project", {
  individual <- globalTestMoBiProject$getIndividual("DefaultIndividual")
  expect_true(isOfType(individual, "BuildingBlock"))
  expect_true(individual$type == BuildingBlockTypes$Individual)
  expect_equal(individual$name, "DefaultIndividual")

  # Test for non-existing individual
  expect_error(
    globalTestMoBiProject$getIndividual("NonExistingIndividual"),
    messages$errorIndividualNotFound("NonExistingIndividual")
  )
})

test_that("It correctly handles non-existing individuals", {
  # Test for non-existing individual with stopIfNotFound = FALSE
  individual <- globalTestMoBiProject$getIndividual(
    "NonExistingIndividual",
    stopIfNotFound = FALSE
  )
  expect_null(individual)
})

# Test for MoBiProject$getSimulation
test_that("It returns a simulation object for getSimulation with existing simulation", {
  simulation <- globalTestMoBiProject$getSimulation("TestSim_2Modules")
  expect_true(isOfType(simulation, "Simulation"))
  expect_equal(simulation$name, "TestSim_2Modules")

  # Test for non-existing simulation
  expect_error(
    globalTestMoBiProject$getSimulation("NonExistingSimulation"),
    messages$errorSimulationNotFound("NonExistingSimulation"),
    fixed = TRUE
  )
})

test_that("It correctly handles non-existing simulations", {
  # Test for non-existing simulation with stopIfNotFound = FALSE
  simulation <- globalTestMoBiProject$getSimulation(
    "NonExistingSimulation",
    stopIfNotFound = FALSE
  )
  expect_null(simulation)
})

# Test for MoBiProject$getExpressionProfiles
test_that("It can get expression profiles from a MoBi project", {
  expressionProfiles <- globalTestMoBiProject$getExpressionProfiles(c(
    "UGT2B7|Human|Healthy"
  ))
  expect_true(isOfType(expressionProfiles, "BuildingBlock"))
  expect_true(
    expressionProfiles[[1]]$type == BuildingBlockTypes$`Expression Profile`
  )
  expect_equal(
    names(expressionProfiles),
    c("UGT2B7|Human|Healthy")
  )

  # Test for non-existing expression profile
  expect_error(
    globalTestMoBiProject$getExpressionProfiles(names = "NonExistingProfile"),
    messages$errorExpressionProfileNotFound(c("NonExistingProfile")),
    fixed = TRUE
  )

  # Test for a list of expression profiles where one profile is existent
  expect_error(
    globalTestMoBiProject$getExpressionProfiles(
      names = c(
        "NonExistingProfile",
        "UGT2B7|Human|Healthy",
        "nonExistingTheSecond"
      )
    ),
    messages$errorExpressionProfileNotFound(
      names = c("NonExistingProfile", "nonExistingTheSecond")
    ),
    fixed = TRUE
  )
})

test_that("It correctly handles non-existing expression profiles", {
  # Test for non-existing expression profile with stopIfNotFound = FALSE
  expressionProfiles <- globalTestMoBiProject$getExpressionProfiles(
    c("NonExistingProfile"),
    stopIfNotFound = FALSE
  )
  expect_vector(expressionProfiles, list(), size = 0)

  expressionProfiles <- globalTestMoBiProject$getExpressionProfiles(
    names = c(
      "NonExistingProfile",
      "UGT2B7|Human|Healthy",
      "nonExistingTheSecond"
    ),
    stopIfNotFound = FALSE
  )
  expect_equal(names(expressionProfiles), "UGT2B7|Human|Healthy")
})

# Test for MoBiProject$getDataSets
test_that("It can get all data sets from a MoBi project", {
  allDataSetNames <- c(
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A",
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_133 mg/kg/day_po_Fig.5A"
  )
  dataSets <- globalTestMoBiProject$getObservedData()
  expect_true(isOfType(dataSets, "DataSet"))
  expect_named(dataSets, allDataSetNames, ignore.order = TRUE)

  # Test for empty project
  emptyDataSets <- emptyProject$getObservedData()
  expect_length(emptyDataSets, 0)
})

test_that("It can get specific data sets from a MoBi project", {
  dataSetNames <- c(
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A"
  )
  dataSets <- globalTestMoBiProject$getObservedData(dataSetNames)
  expect_true(isOfType(dataSets, "DataSet"))
  expect_equal(names(dataSets), dataSetNames)

  # Test for non-existing data set
  expect_error(
    globalTestMoBiProject$getObservedData("NonExistingDataSet"),
    messages$errorDataSetsNotPresentInProject(c("NonExistingDataSet")),
    fixed = TRUE
  )

  # Test for a list of data sets where one is non-existing
  expect_error(
    globalTestMoBiProject$getObservedData(c(
      "NonExistingDataSet",
      "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A"
    )),
    messages$errorDataSetsNotPresentInProject(c("NonExistingDataSet")),
    fixed = TRUE
  )
})

test_that("It correctly handles non-existing data sets", {
  # Test for non-existing data set with stopIfNotFound = FALSE
  dataSets <- globalTestMoBiProject$getObservedData(
    "NonExistingDataSet",
    stopIfNotFound = FALSE
  )
  expect_length(dataSets, 0)

  # Test for a list of data sets where one is non-existing
  dataSets <- globalTestMoBiProject$getObservedData(
    c(
      "NonExistingDataSet",
      "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A"
    ),
    stopIfNotFound = FALSE
  )
  expect_equal(
    names(dataSets),
    "Liu 1995_Total T3__Rat__VenousBlood_Plasma_0 mg/kg/day_po_Fig.5A"
  )
})

# Test for MoBiProject$createSimulationConfiguration

test_that("It can create a simulation configuration with default IC and PV BBs", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = "ExtModule_3IC_3PV"
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))

  # Default IC and PV should be the first ones (IC1, PV1)
  expect_equal(
    simConfig$selectedInitialConditions,
    list("ExtModule_3IC_3PV" = "IC1")
  )
  expect_equal(
    simConfig$selectedParameterValues,
    list("ExtModule_3IC_3PV" = "PV1")
  )

  # No individual or expression profiles specified
  expect_null(simConfig$individual)
  expect_length(simConfig$expressionProfiles, 0)
})

test_that("It can create a simulation configuration with a module that has no IC or PV BBs", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = "ExtModule_noIC_noPV"
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))

  # No IC or PV BBs should be selected
  expect_equal(
    simConfig$selectedInitialConditions,
    list("ExtModule_noIC_noPV" = NULL)
  )
  expect_equal(
    simConfig$selectedParameterValues,
    list("ExtModule_noIC_noPV" = NULL)
  )
})

test_that("It can create a simulation configuration with multiple modules", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = c("ExtModule_3IC_3PV", "ExtModule_noIC_noPV")
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))

  expect_equal(
    simConfig$selectedInitialConditions,
    list("ExtModule_3IC_3PV" = "IC1", "ExtModule_noIC_noPV" = NULL)
  )
  expect_equal(
    simConfig$selectedParameterValues,
    list("ExtModule_3IC_3PV" = "PV1", "ExtModule_noIC_noPV" = NULL)
  )
})

test_that("It can create a simulation configuration with an individual", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = "ExtModule_3IC_3PV",
    individualName = "DefaultIndividual"
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))
  expect_equal(simConfig$individual$name, "DefaultIndividual")
})

test_that("It can create a simulation configuration with expression profiles", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = "ExtModule_3IC_3PV",
    expressionProfilesNames = c("UGT2B7|Human|Healthy", "CYP3A4|Human|Healthy")
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))
  expect_named(
    simConfig$expressionProfiles,
    c("UGT2B7|Human|Healthy", "CYP3A4|Human|Healthy"),
    ignore.order = TRUE
  )
})

test_that("It can create a simulation configuration with specific IC and PV BBs", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = "ExtModule_3IC_3PV",
    selectedInitialConditions = list("ExtModule_3IC_3PV" = "IC2"),
    selectedParameterValues = list("ExtModule_3IC_3PV" = "PV3")
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))
  expect_equal(
    simConfig$selectedInitialConditions,
    list("ExtModule_3IC_3PV" = "IC2")
  )
  expect_equal(
    simConfig$selectedParameterValues,
    list("ExtModule_3IC_3PV" = "PV3")
  )
})

test_that("It can create a simulation configuration with IC and PV explicitly set to NULL", {
  simConfig <- globalTestMoBiProject$createSimulationConfiguration(
    modulesNames = "ExtModule_3IC_3PV",
    selectedInitialConditions = list("ExtModule_3IC_3PV" = NULL),
    selectedParameterValues = list("ExtModule_3IC_3PV" = NULL)
  )
  expect_true(isOfType(simConfig, "SimulationConfiguration"))
  expect_equal(
    simConfig$selectedInitialConditions,
    list("ExtModule_3IC_3PV" = NULL)
  )
  expect_equal(
    simConfig$selectedParameterValues,
    list("ExtModule_3IC_3PV" = NULL)
  )
})

test_that("It throws an error when creating a configuration with non-existing modules", {
  expect_error(
    globalTestMoBiProject$createSimulationConfiguration(
      modulesNames = "NonExistingModule"
    ),
    messages$modulesNotPresentInProject("NonExistingModule"),
    fixed = TRUE
  )
})

test_that("It throws an error when creating a configuration with a non-existing individual", {
  expect_error(
    globalTestMoBiProject$createSimulationConfiguration(
      modulesNames = "ExtModule_3IC_3PV",
      individualName = "NonExistingIndividual"
    ),
    messages$errorIndividualNotFound("NonExistingIndividual")
  )
})

test_that("It throws an error when creating a configuration with non-existing expression profiles", {
  expect_error(
    globalTestMoBiProject$createSimulationConfiguration(
      modulesNames = "ExtModule_3IC_3PV",
      expressionProfilesNames = "NonExistingProfile"
    ),
    messages$errorExpressionProfileNotFound("NonExistingProfile"),
    fixed = TRUE
  )
})

test_that("It throws an error when specifying a non-existing IC BB for a module", {
  expect_error(
    globalTestMoBiProject$createSimulationConfiguration(
      modulesNames = "ExtModule_3IC_3PV",
      selectedInitialConditions = list("ExtModule_3IC_3PV" = "NonExistingIC")
    ),
    regexp = messages$errorICNotFoundInModule(
      "NonExistingIC",
      "ExtModule_3IC_3PV"
    ),
    fixed = TRUE
  )
})

test_that("It throws an error when specifying a non-existing PV BB for a module", {
  expect_error(
    globalTestMoBiProject$createSimulationConfiguration(
      modulesNames = "ExtModule_3IC_3PV",
      selectedParameterValues = list("ExtModule_3IC_3PV" = "NonExistingPV")
    ),
    messages$errorPVNotFoundInModule("NonExistingPV", "ExtModule_3IC_3PV"),
    fixed = TRUE
  )
})
