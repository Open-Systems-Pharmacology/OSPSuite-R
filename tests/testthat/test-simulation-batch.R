tolerance <- 0.0001
sim <- loadTestSimulation("simple", loadFromCache = TRUE)

test_that("It returns correct lists for variable parameters or molecules, or NULL
          when no variable entities are specified", {
  # Get the paths of all molecules in the simulation
  moleculePaths <- getAllMoleculePathsIn(container = sim)
  # Get the paths of all parameters
  parameterPaths <- getAllParameterPathsIn(container = sim)

  # Create simulation batch and define all parameters as variable
  simBatch <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameterPaths
  )

  expect_null(simBatch$getVariableMolecules())
  expect_equal(simBatch$getVariableParameters(), parameterPaths)

  # Create simulation batch and define all molecules as variable
  simBatch <- createSimulationBatch(
    simulation = sim,
    moleculesOrPaths = moleculePaths
  )

  expect_null(simBatch$getVariableParameters())
  expect_equal(simBatch$getVariableMolecules(), moleculePaths)
})

test_that("It throws an error when any of the added molecule start values is NaN", {
  # Get the paths of all molecules in the simulation
  moleculePaths <- getAllMoleculePathsIn(container = sim)
  # Get the current initial values of all molecules
  moleculesStartValues <- getQuantityValuesByPath(
    quantityPaths = moleculePaths,
    simulation = sim
  )
  # Replace one value by NaN
  moleculesStartValues[[1]] <- NaN
  # Create simulation batch and define all molecules as variable
  simBatch <- createSimulationBatch(
    simulation = sim,
    moleculesOrPaths = moleculePaths
  )
  # Add run values
  expect_error(simBatch$addRunValues(initialValues = moleculesStartValues),
    regexp = messages$simBatchStartValueNaN(moleculePaths[[1]])
  )
})

test_that("It throws an error when any of the added parameter values is NaN", {
  # Get the paths of all parameters
  parameterPaths <- getAllParameterPathsIn(container = sim)
  # Get the current initial values of all molecules
  parameterValues <- getQuantityValuesByPath(
    quantityPaths = parameterPaths,
    simulation = sim
  )
  # Replace one value by NaN
  parameterValues[[1]] <- NaN
  # Create simulation batch and define all molecules as variable
  simBatch <- createSimulationBatch(
    simulation = sim,
    parametersOrPaths = parameterPaths
  )
  # Add run values
  expect_error(simBatch$addRunValues(parameterValues = parameterValues),
    regexp = messages$simBatchStartValueNaN(parameterPaths[[1]])
  )
})

test_that("It actually sets new initial values", {
  # set output interval to one minute only
  setOutputInterval(
    simulation = sim,
    startTime = 0,
    endTime = 1,
    resolution = 1
  )
  # Get the paths of all molecules in the simulation
  moleculePaths <- getAllMoleculePathsIn(container = sim)
  # Get the current initial values of all molecules
  moleculesStartValues <- getQuantityValuesByPath(
    quantityPaths = moleculePaths,
    simulation = sim
  )
  # Update start values by adding an arbitrary number
  moleculesStartValues <- moleculesStartValues + 1
  # Add all molecule paths as outputs
  addOutputs(quantitiesOrPaths = moleculePaths, simulation = sim)

  # Create simulation batch and define all molecules as variable
  simBatch <- createSimulationBatch(
    simulation = sim,
    moleculesOrPaths = moleculePaths
  )
  # Add run values
  simBatch$addRunValues(initialValues = moleculesStartValues)

  results <- runSimulationBatches(simBatch)
  # values at simulation start should correspond to the updated start values
  outputValues <- getOutputValues(
    simulationResults = results[[1]][[1]],
    quantitiesOrPaths = moleculePaths
  )

  expect_equal(unlist(outputValues$data[moleculePaths][1, ], use.names = FALSE), moleculesStartValues,
    tolerance = tolerance
  )
})
