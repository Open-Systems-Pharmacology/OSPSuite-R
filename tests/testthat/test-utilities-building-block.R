getSimulation <- function(loadFromCache = FALSE) {
  loadSimulation(
    aciclovirSimulationPath,
    loadFromCache = loadFromCache,
    addToCache = loadFromCache
  )
}

getSimpleProject <- function() {
  loadMoBiProject(getTestDataFilePath("MoBiProject/Test_Project.mbp3"))
}

# Reloads simulation and gets the first initial conditions building block. This is needed to ensure that we have a fresh instance of the building block for each test, as the building blocks are mutable and changes in one test could affect other tests.
getFreshICBB <- function(simulation = getSimulation()) {
  simulation$configuration$modules[[1]]$getInitialConditionsBBs()[[1]]
}

getFreshPVBB <- function(simulation = getSimulation()) {
  simulation$configuration$modules[[1]]$getParameterValuesBBs()[[1]]
}

# Used for non-mutable tests that just need to read the building blocks without modifying them. This avoids the overhead of loading the simulation multiple times.
cachedICBB <- getFreshICBB()
cachedPVBB <- getFreshPVBB()

test_that("initialConditionsBBToDataFrame returns a data frame with the expected columns", {
  df <- initialConditionsBBToDataFrame(cachedICBB)
  expect_snapshot(df)
})

test_that("initialConditionsBBToDataFrame throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    initialConditionsBBToDataFrame(cachedPVBB),
    regexp = "Initial Conditions"
  )
})

test_that("parameterValuesBBToDataFrame returns a data frame with the expected columns", {
  df <- parameterValuesBBToDataFrame(cachedPVBB)
  expect_snapshot(df)
})

test_that("parameterValuesBBToDataFrame throws an error if the building block is not of type Parameter Values", {
  expect_error(
    parameterValuesBBToDataFrame(cachedICBB),
    regexp = "Parameter Values"
  )
})

# test for wrong type of building block
test_that("setInitialConditionsInBB throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    setInitialConditionsInBB(cachedPVBB, "Path", 1),
    regexp = "Initial Conditions"
  )
})

# test for differnt lengths of quantityPaths and quantityValues
test_that("setInitialConditionsInBB throws an error if quantityPaths and quantityValues have different lengths", {
  expect_error(
    setInitialConditionsInBB(
      cachedICBB,
      quantityPaths = c("Path1", "Path2"),
      quantityValues = c(1)
    ),
    regexp = "The length of quantityPaths should be equal to the length "
  )
})

test_that("setInitialConditionsInBB updates one entry correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- "Organism|Brain|Intracellular|Aciclovir"
  quantityValues <- 10

  # Set initial conditions
  setInitialConditionsInBB(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 1,
    isPresent = TRUE,
    negativeValuesAllowed = FALSE
  )

  df <- initialConditionsBBToDataFrame(icBB)

  # Check if the paths exist in the data frame
  # The data frame columns are "Container Path" and "Molecule Name"
  brainEntry <- df[
    df$`Container Path` == "Organism|Brain|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]

  expect_equal(nrow(brainEntry), 1)
  expect_equal(brainEntry$Value, 10)
})

# It can change two entries of the same molecule in different compartments
test_that("setInitialConditionsInBB updates multiple entries correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- c(
    "Organism|Brain|Intracellular|Aciclovir",
    "Organism|Kidney|Intracellular|Aciclovir"
  )
  quantityValues <- c(10, 20)

  # Set initial conditions
  setInitialConditionsInBB(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 2,
    isPresent = TRUE,
    negativeValuesAllowed = TRUE
  )

  df <- initialConditionsBBToDataFrame(icBB)

  brainEntry <- df[
    df$`Container Path` == "Organism|Brain|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]
  kidneyEntry <- df[
    df$`Container Path` == "Organism|Kidney|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]

  expect_equal(nrow(brainEntry), 1)
  expect_equal(nrow(kidneyEntry), 1)
  expect_equal(brainEntry$Value, 10)
  expect_equal(brainEntry$`Scale Divisor`, 2)
  expect_equal(brainEntry$`Is Present`, TRUE)
  expect_equal(brainEntry$`Neg. Values Allowed`, TRUE)
  expect_equal(kidneyEntry$Value, 20)
  expect_equal(kidneyEntry$`Scale Divisor`, 2)
  expect_equal(kidneyEntry$`Is Present`, TRUE)
  expect_equal(kidneyEntry$`Neg. Values Allowed`, TRUE)
})

# It can add new entries if the paths do not exist in the BB
test_that("setInitialConditionsInBB adds new entries correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- c(
    "Organism|Intracellular|Aciclovir",
    "Organism|Heart|New|Aciclovir"
  )
  quantityValues <- c(30, 40)

  # Set initial conditions
  setInitialConditionsInBB(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 1,
    isPresent = TRUE,
    negativeValuesAllowed = FALSE
  )

  df <- initialConditionsBBToDataFrame(icBB)
  liverEntry <- df[
    df$`Container Path` == "Organism|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]
  heartEntry <- df[
    df$`Container Path` == "Organism|Heart|New" &
      df$`Molecule Name` == "Aciclovir",
  ]

  expect_equal(nrow(liverEntry), 1)
  expect_equal(nrow(heartEntry), 1)
  expect_equal(liverEntry$Value, 30)
  expect_equal(heartEntry$Value, 40)
})

test_that("deleteInitialConditionsFromBB throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    deleteInitialConditionsFromBB(
      cachedPVBB,
      "Organism|Brain|Intracellular|Aciclovir"
    ),
    regexp = "Initial Conditions"
  )
})

test_that("deleteInitialConditionsFromBB removes existing entries correctly", {
  icBB <- getFreshICBB()
  # Get initial count
  dfBefore <- initialConditionsBBToDataFrame(icBB)
  pathToDelete <- "Organism|Brain|Intracellular|Aciclovir"

  # Ensure the path exists before deletion
  expect_true(any(
    paste0(dfBefore$`Container Path`, "|", dfBefore$`Molecule Name`) ==
      pathToDelete
  ))

  # Delete the entry
  deleteInitialConditionsFromBB(icBB, pathToDelete)

  # Check after deletion
  dfAfter <- initialConditionsBBToDataFrame(icBB)
  expect_false(any(
    paste0(dfAfter$`Container Path`, "|", dfAfter$`Molecule Name`) ==
      pathToDelete
  ))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 1)
})

test_that("deleteInitialConditionsFromBB removes multiple entries correctly", {
  icBB <- getFreshICBB()
  dfBefore <- initialConditionsBBToDataFrame(icBB)
  pathsToDelete <- c(
    "Organism|Kidney|Intracellular|Aciclovir",
    "Organism|Muscle|Intracellular|Aciclovir"
  )

  deleteInitialConditionsFromBB(icBB, pathsToDelete)

  dfAfter <- initialConditionsBBToDataFrame(icBB)
  currentPaths <- paste0(dfAfter$`Container Path`, "|", dfAfter$`Molecule Name`)

  expect_false(any(currentPaths %in% pathsToDelete))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 2)
})

test_that("deleteInitialConditionsFromBB ignores paths that do not exist", {
  icBB <- getFreshICBB()
  dfBefore <- initialConditionsBBToDataFrame(icBB)
  nonExistentPath <- "Non|Existent|Path|Molecule"

  # Should not throw an error
  expect_silent(deleteInitialConditionsFromBB(icBB, nonExistentPath))

  dfAfter <- initialConditionsBBToDataFrame(icBB)
  expect_equal(dfAfter, dfBefore)
})

# extendInitialConditions
test_that("extendInitialConditionsBB extends with all molecules if moleculeNames is NULL", {
  # BB to mutate
  icBB <- getFreshICBB()
  # Module to get BBs from that will not be mutated
  module <- globalTestMoBiProject$getModules("TestModule")[[1]]
  spatialStructureModule <- loadModuleFromPKML(system.file(
    "extdata",
    "Thyroid.pkml",
    package = "ospsuite"
  ))

  newPaths <- extendInitialConditionsBB(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = spatialStructureModule,
    moleculesModule = module
  )

  icBB_df <- initialConditionsBBToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditionsBB does not add new entries for existing molecules and compartments", {
  simulation <- getSimulation()
  module <- simulation$configuration$modules[[1]]
  icBB <- module$getInitialConditionsBBs()[[1]]

  newPaths <- extendInitialConditionsBB(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = module,
    moleculesModule = module,
    moleculeNames = c("Aciclovir", "CYP3A4")
  )

  # New entries are only added for CYP3A4, as Aciclovir already has entries in the IC BB for all compartments in which it is present in the molecules module. For CYP3A4, new entries are only added for compartments in which it is present in the molecules module and does not yet have an entry in the IC BB, which are "Organism|Liver|Intracellular" and "Organism|Intestine|Intracellular".
  icBB_df <- initialConditionsBBToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditionsBB throws error for wrong spatial structure module", {
  moleculesModule <- getSimulation(loadFromCache = TRUE)$configuration$modules[[
    1
  ]]
  # A module without a spatial structure BB is needed.
  emptyModule <- globalTestMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    extendInitialConditionsBB(cachedICBB, emptyModule, moleculesModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditionsBB throws error for wrong molecules module", {
  spatialStructureModule <- getSimulation(
    loadFromCache = TRUE
  )$configuration$modules[[1]]
  emptyModule <- globalTestMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    extendInitialConditionsBB(
      cachedICBB,
      spatialStructureModule,
      emptyModule
    ),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditionsBB should handle wrong type of initialConditionsBuildingBlock", {
  simulation <- getSimulation(loadFromCache = TRUE)
  module <- simulation$configuration$modules[[1]]

  expect_error(
    extendInitialConditionsBB(cachedPVBB, module, module)
  )
})

# setParameterValuesInBB tests

test_that("setParameterValuesInBB throws an error if the building block is not of type Parameter Values", {
  expect_error(
    setParameterValuesInBB(cachedICBB, "Path", 1, units = ""),
    regexp = "Parameter Values"
  )
})

test_that("setParameterValuesInBB throws an error if quantityPaths and quantityValues have different lengths", {
  expect_error(
    setParameterValuesInBB(
      cachedPVBB,
      quantityPaths = c("Path1", "Path2"),
      quantityValues = c(1),
      units = ""
    ),
    regexp = "The length of quantityPaths should be equal to the length "
  )
})

test_that("setParameterValuesInBB sets a single entry with explicit dimensions", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  quantityPath <- "Organism|Liver|Volume"
  quantityValue <- 1.5

  setParameterValuesInBB(
    pvBB,
    quantityPaths = quantityPath,
    quantityValues = quantityValue,
    units = "l",
    dimensions = ospDimensions$Volume
  )

  df <- parameterValuesBBToDataFrame(pvBB)
  entry <- df[
    df$`Container Path` == "Organism|Liver" &
      df$`Parameter Name` == "Volume",
  ]

  expect_equal(nrow(entry), 1)
  expect_equal(entry$Value, 1.5)
})

test_that("setParameterValuesInBB derives dimensions from units when dimensions is NULL", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  quantityPath <- "Organism|Liver|Volume"
  quantityValue <- 1.5

  setParameterValuesInBB(
    pvBB,
    quantityPaths = quantityPath,
    quantityValues = quantityValue,
    units = "l"
  )

  df <- parameterValuesBBToDataFrame(pvBB)
  entry <- df[
    df$`Container Path` == "Organism|Liver" &
      df$`Parameter Name` == "Volume",
  ]

  expect_equal(nrow(entry), 1)
  expect_equal(entry$Value, 1.5)
})

test_that("setParameterValuesInBB sets multiple entries correctly", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  quantityPaths <- c(
    "Organism|Liver|Volume",
    "Organism|Kidney|Volume"
  )
  quantityValues <- c(1.5, 0.3)

  setParameterValuesInBB(
    pvBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    units = "l",
    dimensions = ospDimensions$Volume
  )

  df <- parameterValuesBBToDataFrame(pvBB)
  liverEntry <- df[
    df$`Container Path` == "Organism|Liver" &
      df$`Parameter Name` == "Volume",
  ]
  kidneyEntry <- df[
    df$`Container Path` == "Organism|Kidney" &
      df$`Parameter Name` == "Volume",
  ]

  expect_equal(nrow(liverEntry), 1)
  expect_equal(nrow(kidneyEntry), 1)
  expect_equal(liverEntry$Value, 1.5)
  expect_equal(kidneyEntry$Value, 0.3)
})

test_that("setParameterValuesInBB overwrites the dimension of an existing entry", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  quantityPaths <- c(
    "Organism|Liver|Volume"
  )
  quantityValues <- c(1.5)

  setParameterValuesInBB(
    pvBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    units = "l",
    dimensions = ospDimensions$Volume
  )

  setParameterValuesInBB(
    pvBB,
    quantityPaths = quantityPaths,
    quantityValues = 1,
    units = "µmol",
    dimensions = ospDimensions$Amount
  )

  df <- parameterValuesBBToDataFrame(pvBB)
  liverEntry <- df[
    df$`Container Path` == "Organism|Liver" &
      df$`Parameter Name` == "Volume",
  ]
  expect_equal(liverEntry$Unit, "µmol")
})

test_that("setParameterValuesInBB correctly updates an existing entry when the values are provided in non-base units", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  quantityPaths <- c(
    "Organism|Heart|Volume"
  )

  # Set the value in non-base units. The value should be converted to the base unit and overwrite the existing value.
  setParameterValuesInBB(
    pvBB,
    quantityPaths = "Organism|Heart|Volume",
    quantityValues = 0.1,
    units = "ml"
  )

  df <- parameterValuesBBToDataFrame(pvBB)
  heartEntry <- df[
    df$`Container Path` == "Organism|Heart" &
      df$`Parameter Name` == "Volume",
  ]
  # The value should be converted to liters, so 0.1 ml = 0.0001 l
  expect_equal(heartEntry$Value, 0.0001)
})

# deleteParameterValuesFromBB tests

test_that("deleteParameterValuesFromBB throws an error if the building block is not of type Parameter Values", {
  expect_error(
    deleteParameterValuesFromBB(cachedICBB, "Path"),
    regexp = "Parameter Values"
  )
})

test_that("deleteParameterValuesFromBB removes existing entries correctly", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  # The PV BB in this simulation is empty, so we have to add some entries first
  setParameterValuesInBB(
    pvBB,
    quantityPaths = c(
      "Organism|Liver|Volume",
      "Organism|Kidney|Volume"
    ),
    quantityValues = c(1.5, 0.3),
    units = "l",
    dimensions = ospDimensions$Volume
  )

  dfBefore <- parameterValuesBBToDataFrame(pvBB)
  pathToDelete <- "Organism|Liver|Volume"
  deleteParameterValuesFromBB(pvBB, pathToDelete)

  dfAfter <- parameterValuesBBToDataFrame(pvBB)
  expect_false(any(
    paste0(dfAfter$`Container Path`, "|", dfAfter$`Parameter Name`) ==
      pathToDelete
  ))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 1)
})

test_that("deleteParameterValuesFromBB removes multiple entries correctly", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  # The PV BB in this simulation is empty, so we have to add some entries first
  setParameterValuesInBB(
    pvBB,
    quantityPaths = c(
      "Organism|Liver|Volume",
      "Organism|Kidney|Volume",
      "Organism|Heart|Weight"
    ),
    quantityValues = c(1.5, 0.3, 0.8),
    units = c("l", "l", "g")
  )
  dfBefore <- parameterValuesBBToDataFrame(pvBB)

  pathsToDelete <- paste0(
    dfBefore$`Container Path`[1:2],
    "|",
    dfBefore$`Parameter Name`[1:2]
  )

  deleteParameterValuesFromBB(pvBB, pathsToDelete)

  dfAfter <- parameterValuesBBToDataFrame(pvBB)
  currentPaths <- paste0(
    dfAfter$`Container Path`,
    "|",
    dfAfter$`Parameter Name`
  )

  expect_false(any(currentPaths %in% pathsToDelete))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 2)
})

test_that("deleteParameterValuesFromBB ignores paths that do not exist", {
  simpleProject <- getSimpleProject()
  pvBB <- simpleProject$getModules("TestModule")[[1]]$getParameterValuesBBs()[[
    1
  ]]
  # The PV BB in this simulation is empty, so we have to add some entries first
  setParameterValuesInBB(
    pvBB,
    quantityPaths = c(
      "Organism|Liver|Volume",
      "Organism|Kidney|Volume"
    ),
    quantityValues = c(1.5, 0.3),
    units = "l",
    dimensions = ospDimensions$Volume
  )
  dfBefore <- parameterValuesBBToDataFrame(pvBB)
  nonExistentPath <- "Non|Existent|Path|Parameter"

  expect_silent(deleteParameterValuesFromBB(pvBB, nonExistentPath))

  dfAfter <- parameterValuesBBToDataFrame(pvBB)
  expect_equal(dfAfter, dfBefore)
})

# addLocalMoleculeParametersToParameterValuesBB tests

test_that("addLocalMoleculeParametersToParameterValuesBB adds parameters for all molecules when moleculeNames is NULL", {
  module <- getSimpleProject()$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]

  newPaths <- addLocalMoleculeParametersToParameterValuesBB(
    parameterValuesBuildingBlock = pvBB,
    spatialStructureModule = module,
    moleculesModule = module
  )

  pvBB_df <- parameterValuesBBToDataFrame(pvBB)
  newPaths_df <- pvBB_df[
    paste0(pvBB_df$`Container Path`, "|", pvBB_df$`Parameter Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("addLocalMoleculeParametersToParameterValuesBB adds parameters for multiple specified molecules", {
  testProject <- getSimpleProject
  module <- testProject$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]

  newPaths <- addLocalMoleculeParametersToParameterValuesBB(
    parameterValuesBuildingBlock = pvBB,
    spatialStructureModule = module,
    moleculesModule = module,
    moleculeNames = c("A", "B")
  )

  pvBB_df <- parameterValuesBBToDataFrame(pvBB)
  newPaths_df <- pvBB_df[
    paste0(pvBB_df$`Container Path`, "|", pvBB_df$`Parameter Name`) %in%
      newPaths,
  ]

  # All new entries should be for molecules A and B
  expect_gt(length(newPaths), 0)
  expect_true(any(grepl("\\|A\\|", newPaths)))
  expect_true(any(grepl("\\|B\\|", newPaths)))
  expect_true(all(grepl("\\|A\\|", newPaths) | grepl("\\|B\\|", newPaths)))
})

test_that("addLocalMoleculeParametersToParameterValuesBB adds parameters only for specified molecules", {
  testProject <- getSimpleProject()
  module <- testProject$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]

  newPaths <- addLocalMoleculeParametersToParameterValuesBB(
    parameterValuesBuildingBlock = pvBB,
    spatialStructureModule = module,
    moleculesModule = module,
    moleculeNames = "A"
  )

  pvBB_df <- parameterValuesBBToDataFrame(pvBB)
  newPaths_df <- pvBB_df[
    paste0(pvBB_df$`Container Path`, "|", pvBB_df$`Parameter Name`) %in%
      newPaths,
  ]

  # All new entries should be for molecule A only
  expect_gt(length(newPaths), 0)
  expect_true(all(grepl("\\|A\\|", newPaths)))
  expect_false(any(grepl("\\|B\\|", newPaths)))

  expect_snapshot(newPaths_df)
})

test_that("addLocalMoleculeParametersToParameterValuesBB does not overwrite existing entries", {
  testProject <- getSimpleProject()
  module <- testProject$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]

  # First call adds entries
  firstPaths <- addLocalMoleculeParametersToParameterValuesBB(
    parameterValuesBuildingBlock = pvBB,
    spatialStructureModule = module,
    moleculesModule = module
  )
  # Set the values for all entries
  setParameterValuesInBB(
    pvBB,
    quantityPaths = firstPaths,
    quantityValues = rep(1, length(firstPaths)),
    units = "µmol"
  )

  # Second call should not add duplicate entries or change existing entries, so it should return an empty vector
  secondPaths <- addLocalMoleculeParametersToParameterValuesBB(
    parameterValuesBuildingBlock = pvBB,
    spatialStructureModule = module,
    moleculesModule = module
  )

  expect_equal(length(secondPaths), 0)
  # The values of the first entries should remain unchanged
  pvBB_df <- parameterValuesBBToDataFrame(pvBB)
  firstPaths_df <- pvBB_df[
    paste0(pvBB_df$`Container Path`, "|", pvBB_df$`Parameter Name`) %in%
      firstPaths,
  ]
  expect_true(all(firstPaths_df$Value == 1))
})

test_that("addLocalMoleculeParametersToParameterValuesBB throws error for wrong BB type", {
  module <- globalTestMoBiProject$getModules("TestModule")[[1]]

  expect_error(
    addLocalMoleculeParametersToParameterValuesBB(cachedICBB, module, module),
    regexp = "Parameter Values"
  )
})

test_that("addLocalMoleculeParametersToParameterValuesBB throws error for module without spatial structure", {
  module <- globalTestMoBiProject$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]
  emptyModule <- globalTestMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    addLocalMoleculeParametersToParameterValuesBB(pvBB, emptyModule, module),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("addLocalMoleculeParametersToParameterValuesBB throws error for module without molecules", {
  module <- globalTestMoBiProject$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]
  emptyModule <- globalTestMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    addLocalMoleculeParametersToParameterValuesBB(pvBB, module, emptyModule),
    "The provided modules do not contain the required building blocks"
  )
})

# Test for molecules that are not present in the project
test_that("addLocalMoleculeParametersToParameterValuesBB ignores molecules that are not present in the molecules module", {
  testProject <- loadMoBiProject(getTestDataFilePath("Test_Project.mbp3"))
  module <- testProject$getModules("TestModule")[[1]]
  pvBB <- module$getParameterValuesBBs()[[1]]

  newPaths <- addLocalMoleculeParametersToParameterValuesBB(
    parameterValuesBuildingBlock = pvBB,
    spatialStructureModule = module,
    moleculesModule = module,
    moleculeNames = c("A", "NonExistentMolecule")
  )

  pvBB_df <- parameterValuesBBToDataFrame(pvBB)
  newPaths_df <- pvBB_df[
    paste0(pvBB_df$`Container Path`, "|", pvBB_df$`Parameter Name`) %in%
      newPaths,
  ]

  # All new entries should be for molecule A only
  expect_gt(length(newPaths), 0)
  expect_true(all(grepl("\\|A\\|", newPaths)))
  expect_false(any(grepl("\\|NonExistentMolecule\\|", newPaths)))
})
