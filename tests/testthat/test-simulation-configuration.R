simulation <- loadSimulation(system.file(
  "extdata",
  "Aciclovir.pkml",
  package = "ospsuite"
))

testMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("Test_Project.mbp3")
)

test_that("SimulationConfiguration can be created from a simulation loaded from PKML", {
  configurationFromPKML <- simulation$configuration
  expect_true(isOfType(configurationFromPKML, "SimulationConfiguration"))

  # Check that modules are correct
  modules <- configurationFromPKML$modules
  expect_true(isOfType(modules, "MoBiModule"))
  expect_named(modules, c("Vergin 1995 IV"))

  # Check that the individual is correct
  expect_null(configurationFromPKML$individual)
  # Check that expression profiles are correct
  expect_null(configurationFromPKML$expressionProfiles)

  # Check that selected initial conditions are correct
  selectedICs <- configurationFromPKML$selectedInitialConditions
  expect_equal(selectedICs, list("Vergin 1995 IV" = "Vergin 1995 IV"))

  # Check that selected parameter values are correct
  selectedPVs <- configurationFromPKML$selectedParameterValues
  expect_equal(selectedPVs, list("Vergin 1995 IV" = "Vergin 1995 IV"))

  expect_snapshot(configurationFromPKML)
})

test_that("SimulationConfiguration can be created from a simulation loaded from a MoBi project with selected IC and PV BBs", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration

  expect_true(isOfType(configurationFromProject, "SimulationConfiguration"))

  # Check that modules are correct
  modules <- configurationFromProject$modules
  expect_true(isOfType(modules, "MoBiModule"))
  expect_named(modules, c("ExtModule_noIC_noPV", "ExtModule_3IC_3PV"))

  # Check that the individual is correct
  expect_equal(configurationFromProject$individual$name, "DefaultIndividual")
  # Check that expression profiles are correct
  expect_true(isOfType(
    configurationFromProject$expressionProfiles,
    "BuildingBlock"
  ))
  expect_named(
    configurationFromProject$expressionProfiles,
    expected = c("UGT2B6|Human|Healthy", "CYP3A4|Human|Healthy"),
    ignore.order = TRUE
  )

  # Check that selected initial conditions are correct
  selectedICs <- configurationFromProject$selectedInitialConditions
  expect_equal(
    selectedICs,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = "IC1")
  )

  # Check that selected parameter values are correct
  selectedPVs <- configurationFromProject$selectedParameterValues
  expect_equal(
    selectedPVs,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = "PV1")
  )

  expect_snapshot(configurationFromProject)
})

test_that("SimulationConfiguration can be created from a simulation loaded from a MoBi project with selected PV but no IC BBs", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules_noICSelected"
  )$configuration

  expect_true(isOfType(configurationFromProject, "SimulationConfiguration"))

  # Check that modules are correct
  modules <- configurationFromProject$modules
  expect_true(isOfType(modules, "MoBiModule"))
  expect_named(modules, c("ExtModule_noIC_noPV", "ExtModule_3IC_3PV"))

  # Check that the individual is correct
  expect_equal(configurationFromProject$individual$name, "DefaultIndividual")
  # Check that expression profiles are correct
  expect_true(isOfType(
    configurationFromProject$expressionProfiles,
    "BuildingBlock"
  ))
  expect_named(
    configurationFromProject$expressionProfiles,
    expected = c("UGT2B6|Human|Healthy", "CYP3A4|Human|Healthy"),
    ignore.order = TRUE
  )

  # Check that selected initial conditions are correct
  selectedICs <- configurationFromProject$selectedInitialConditions
  expect_equal(
    selectedICs,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = NULL)
  )

  # Check that selected parameter values are correct
  selectedPVs <- configurationFromProject$selectedParameterValues
  expect_equal(
    selectedPVs,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = "PV2")
  )

  expect_snapshot(configurationFromProject)
})

### Test for individual
test_that("SimulationConfiguration can get and set individual", {
  configurationFromPKML <- simulation$configuration

  # Retrieve individual from one configuration and set it to another
  individual <- testMoBiProject$getIndividual("DefaultIndividual")
  configurationFromPKML$individual <- individual
  expect_equal(configurationFromPKML$individual$name, "DefaultIndividual")

  # Set NULL as individual
  configurationFromPKML$individual <- NULL
  expect_null(configurationFromPKML$individual)
})

test_that("SimulationConfiguration individual throws an error when setting multiple individuals", {
  configurationFromPKML <- simulation$configuration

  individual1 <- testMoBiProject$getIndividual("DefaultIndividual")
  expect_error(
    configurationFromPKML$individual <- c(individual1, individual1),
    regexp = "Only one individual can be assigned to a simulation configuration.",
    fixed = TRUE
  )
})

# Error when trying to set a wrong BB
test_that("SimulationConfiguration individual throws an error when wrong BB type is provided for individual", {
  configurationFromPKML <- simulation$configuration
  bb <- testMoBiProject$getExpressionProfiles("CYP3A4|Human|Healthy")[[1]]

  expect_error(
    configurationFromPKML$individual <- bb,
    regexp = "Building Block with the name 'CYP3A4|Human|Healthy' is of type 'Expression Profile', but expected type is 'Individual'",
    fixed = TRUE
  )
})

### Test for Expression Profiles
# it can set and get exp profiles
test_that("SimulationConfiguration can get and set expression profiles", {
  configurationFromPKML <- simulation$configuration

  expProfiles <- testMoBiProject$getExpressionProfiles(c(
    "CYP3A4|Human|Healthy",
    "UGT2B6|Human|Healthy"
  ))

  configurationFromPKML$expressionProfiles <- expProfiles
  expProfiles <- configurationFromPKML$expressionProfiles

  expect_true(isOfType(expProfiles, "BuildingBlock"))
  expect_named(
    expProfiles,
    c("CYP3A4|Human|Healthy", "UGT2B6|Human|Healthy"),
    ignore.order = TRUE
  )
})

# it throws an error when trying to set a profile for the same enzyme multiple times
test_that("SimulationConfiguration expression profiles throws an error when setting multiple profiles for the same protein", {
  configurationFromPKML <- simulation$configuration

  expProfile1 <- testMoBiProject$getExpressionProfiles("CYP3A4|Human|Healthy")[[
    1
  ]]
  expProfile2 <- testMoBiProject$getExpressionProfiles("CYP3A4|Human|Healthy")[[
    1
  ]]

  expect_error(
    configurationFromPKML$expressionProfiles <- list(
      expProfile1,
      expProfile2
    ),
    regexp = "Expression for the protein 'CYP3A4' has already been defined for this simulation configuration with the expression profile 'CYP3A4|Human|Healthy'.",
    fixed = TRUE
  )
})

### Selected Initial Conditions
test_that("SimulationConfiguration throws an error when the passed IC is not a named list", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration

  expect_error(
    configurationFromProject$selectedInitialConditions <- list(
      "ExtModule_noIC_noPV",
      NULL
    ),
    regexp = "The parameter 'selectedInitialConditions' must be a named list"
  )
})

test_that("SimulationConfiguration can get and set selected initial conditions", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration

  originalICs <- configurationFromProject$selectedInitialConditions

  # Set selected IC for one module
  configurationFromProject$selectedInitialConditions <- list(
    "ExtModule_3IC_3PV" = "IC3"
  )
  expect_equal(
    configurationFromProject$selectedInitialConditions,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = "IC3")
  )

  configurationFromProject$selectedInitialConditions <- list(
    "ExtModule_3IC_3PV" = NULL
  )
  expect_equal(
    configurationFromProject$selectedInitialConditions,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = NULL)
  )

  # Reset to original
  configurationFromProject$selectedInitialConditions <- originalICs
  expect_equal(
    configurationFromProject$selectedInitialConditions,
    originalICs
  )
})

# Trying to set a non existing IC BB
test_that("SimulationConfiguration selected initial conditions throws an error when setting a non existing IC BB", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration
  originalICs <- configurationFromProject$selectedInitialConditions

  expect_error(
    configurationFromProject$selectedInitialConditions <- list(
      "ExtModule_3IC_3PV" = "NonExistingIC"
    ),
    regexp = "Initial Condition Building Block with the name 'NonExistingIC' is not present in the module 'ExtModule_3IC_3PV'.",
    fixed = TRUE
  )
  expect_equal(
    configurationFromProject$selectedInitialConditions,
    originalICs
  )
})

# Trying to set an IC BB for a non existing module
test_that("SimulationConfiguration selected initial conditions throws an error when setting an IC BB for a non existing module", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration
  originalICs <- configurationFromProject$selectedInitialConditions

  expect_error(
    configurationFromProject$selectedInitialConditions <- list(
      "ExtModule_3IC_3PV" = NULL,
      "NonExistingModule" = "IC1"
    ),
    regexp = "Module(s) with the name(s) 'NonExistingModule' is not part of the simulation configuration.",
    fixed = TRUE
  )
  expect_equal(
    configurationFromProject$selectedInitialConditions,
    originalICs
  )
})

### Selected Parameter Values
### Selected Parameter Values
test_that("SimulationConfiguration throws an error when the passed PV is not a named list", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration

  expect_error(
    configurationFromProject$selectedParameterValues <- list(
      "ExtModule_noIC_noPV",
      NULL
    ),
    regexp = "The parameter 'selectedParameterValues' must be a named list"
  )
})

test_that("SimulationConfiguration can get and set selected parameter values", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration

  originalPVs <- configurationFromProject$selectedParameterValues

  # Set selected PV for one module
  configurationFromProject$selectedParameterValues <- list(
    "ExtModule_3IC_3PV" = "PV3"
  )
  expect_equal(
    configurationFromProject$selectedParameterValues,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = "PV3")
  )

  # Setting NULL
  configurationFromProject$selectedParameterValues <- list(
    "ExtModule_3IC_3PV" = NULL
  )
  expect_equal(
    configurationFromProject$selectedParameterValues,
    list("ExtModule_noIC_noPV" = NULL, "ExtModule_3IC_3PV" = NULL)
  )

  # Reset to original
  configurationFromProject$selectedParameterValues <- originalPVs
  expect_equal(
    configurationFromProject$selectedParameterValues,
    originalPVs
  )
})

# Trying to set a non existing PV BB
test_that("SimulationConfiguration selected parameter values throws an error when setting a non existing PV BB", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration
  originalPVs <- configurationFromProject$selectedParameterValues

  expect_error(
    configurationFromProject$selectedParameterValues <- list(
      "ExtModule_3IC_3PV" = "NonExistingPV"
    ),
    regexp = "Parameter Values Building Block with the name 'NonExistingPV' is not present in the module 'ExtModule_3IC_3PV'.",
    fixed = TRUE
  )
  expect_equal(
    configurationFromProject$selectedParameterValues,
    originalPVs
  )
})

# Trying to set a PV BB for a non existing module
test_that("SimulationConfiguration selected parameter values throws an error when setting a PV BB for a non existing module", {
  configurationFromProject <- testMoBiProject$getSimulation(
    "TestSim_2Modules"
  )$configuration
  originalPVs <- configurationFromProject$selectedParameterValues

  expect_error(
    configurationFromProject$selectedParameterValues <- list(
      "ExtModule_3IC_3PV" = NULL,
      "NonExistingModule" = "PV1"
    ),
    regexp = "Module(s) with the name(s) 'NonExistingModule' is not part of the simulation configuration.",
    fixed = TRUE
  )
  expect_equal(
    configurationFromProject$selectedParameterValues,
    originalPVs
  )
})
