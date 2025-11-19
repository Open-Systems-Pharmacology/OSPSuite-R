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
    regexp = "Parameter Value Building Block with the name 'NonExistingPV' is not present in the module 'ExtModule_3IC_3PV'.",
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
