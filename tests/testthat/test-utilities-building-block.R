getSimulation <- function() {
  loadSimulation(system.file(
    "extdata",
    "Aciclovir.pkml",
    package = "ospsuite"
  ))
}

getFreshICBB <- function() {
  getSimulation()$configuration$modules[[1]]$getInitialConditionsBBs()[[1]]
}

testPVBB <- getSimulation()$configuration$modules[[1]]$getParameterValuesBBs()[[
  1
]]

getFreshPVBB <- function() {
  getSimulation()$configuration$modules[[1]]$getParameterValuesBBs()[[1]]
}

testProject <- loadMoBiProject(system.file(
  "extdata",
  "TH_QST_Platform.mbp3",
  package = "ospsuite"
))
minimalTestProject <- loadMoBiProject(getTestDataFilePath("Test_Project.mbp3"))

# Test initialConditionsBBToDataFrame with no paths
# Test initialConditionsBBToDataFrame with one path
# Test initialConditionsBBToDataFrame with multiple paths
test_that("initialConditionsBBToDataFrame returns a data frame with the expected columns", {
  icBB <- getFreshICBB()
  df <- initialConditionsBBToDataFrame(icBB)
  expect_snapshot(df)
})

test_that("initialConditionsBBToDataFrame throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    initialConditionsBBToDataFrame(testPVBB),
    regexp = "Initial Conditions"
  )
})

test_that("parameterValuesBBToDataFrame returns a data frame with the expected columns", {
  df <- parameterValuesBBToDataFrame(testPVBB)
  expect_snapshot(df)
})

test_that("parameterValuesBBToDataFrame throws an error if the building block is not of type Parameter Values", {
  icBB <- getFreshICBB()
  expect_error(
    parameterValuesBBToDataFrame(icBB),
    regexp = "Parameter Values"
  )
})


# test for wrong type of building block
test_that("setInitialConditionsBB throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    setInitialConditionsBB(testPVBB, "Path", 1),
    regexp = "Initial Conditions"
  )
})

# test for differnt lengths of quantityPaths and quantityValues
test_that("setInitialConditionsBB throws an error if quantityPaths and quantityValues have different lengths", {
  icBB <- getFreshICBB()
  expect_error(
    setInitialConditionsBB(
      icBB,
      quantityPaths = c("Path1", "Path2"),
      quantityValues = c(1)
    ),
    regexp = "The length of quantityPaths should be equal to the length "
  )
})


test_that("setInitialConditionsBB updates one entry correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- "Organism|Brain|Intracellular|Aciclovir"
  quantityValues <- 10

  # Set initial conditions
  setInitialConditionsBB(
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
test_that("setInitialConditionsBB updates multiple entries correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- c(
    "Organism|Brain|Intracellular|Aciclovir",
    "Organism|Kidney|Intracellular|Aciclovir"
  )
  quantityValues <- c(10, 20)

  # Set initial conditions
  setInitialConditionsBB(
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
test_that("setInitialConditionsBB adds new entries correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- c(
    "Organism|Intracellular|Aciclovir",
    "Organism|Heart|New|Aciclovir"
  )
  quantityValues <- c(30, 40)

  # Set initial conditions
  setInitialConditionsBB(
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

test_that("deleteInitialConditionsBB throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    deleteInitialConditionsBB(
      testPVBB,
      "Organism|Brain|Intracellular|Aciclovir"
    ),
    regexp = "Initial Conditions"
  )
})

test_that("deleteInitialConditionsBB removes existing entries correctly", {
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
  deleteInitialConditionsBB(icBB, pathToDelete)

  # Check after deletion
  dfAfter <- initialConditionsBBToDataFrame(icBB)
  expect_false(any(
    paste0(dfAfter$`Container Path`, "|", dfAfter$`Molecule Name`) ==
      pathToDelete
  ))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 1)
})

test_that("deleteInitialConditionsBB removes multiple entries correctly", {
  icBB <- getFreshICBB()
  dfBefore <- initialConditionsBBToDataFrame(icBB)
  pathsToDelete <- c(
    "Organism|Kidney|Intracellular|Aciclovir",
    "Organism|Muscle|Intracellular|Aciclovir"
  )

  deleteInitialConditionsBB(icBB, pathsToDelete)

  dfAfter <- initialConditionsBBToDataFrame(icBB)
  currentPaths <- paste0(dfAfter$`Container Path`, "|", dfAfter$`Molecule Name`)

  expect_false(any(currentPaths %in% pathsToDelete))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 2)
})

test_that("deleteInitialConditionsBB ignores paths that do not exist", {
  icBB <- getFreshICBB()
  dfBefore <- initialConditionsBBToDataFrame(icBB)
  nonExistentPath <- "Non|Existent|Path|Molecule"

  # Should not throw an error
  expect_silent(deleteInitialConditionsBB(icBB, nonExistentPath))

  dfAfter <- initialConditionsBBToDataFrame(icBB)
  expect_equal(dfAfter, dfBefore)
})

# extendInitialConditions
test_that("extendInitialConditionsBB extends with all molecules if moleculeNames is NULL", {
  module <- testProject$getModules("Thyroid_QST")[[1]]
  icBB <- module$getInitialConditionsBBs()[[1]]
  spatialStructureModule <- thyroidModule <- loadModuleFromPKML(system.file(
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
  module <- testProject$getModules("Thyroid_QST")[[1]]
  icBB <- module$getInitialConditionsBBs()[[1]]

  newPaths <- extendInitialConditionsBB(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = module,
    moleculesModule = module,
    moleculeNames = c("T3", "T4")
  )

  # New entries are still added , specifically for the "EndogenousIgG" compartment. This is expected.
  icBB_df <- initialConditionsBBToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditionsBB extends only with specified molecules", {
  testProject <- loadMoBiProject(system.file(
    "extdata",
    "TH_QST_Platform.mbp3",
    package = "ospsuite"
  ))
  module <- testProject$getModules("Thyroid_QST")[[1]]
  icBB <- module$getInitialConditionsBBs()[[1]]
  spatialStructureModule <- thyroidModule <- loadModuleFromPKML(system.file(
    "extdata",
    "Thyroid.pkml",
    package = "ospsuite"
  ))

  newPaths <- extendInitialConditionsBB(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = spatialStructureModule,
    moleculesModule = module,
    moleculeNames = c("T3", "T4")
  )

  icBB_df <- initialConditionsBBToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditionsBB throws error for wrong spatial structure module", {
  icBB <- getFreshICBB()
  moleculesModule <- getSimulation()$configuration$modules[[1]]
  # A module without a spatial structure BB is needed.
  emptyModule <- minimalTestProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    extendInitialConditionsBB(icBB, emptyModule, moleculesModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditionsBB throws error for wrong molecules module", {
  icBB <- getFreshICBB()
  spatialStructureModule <- getSimulation()$configuration$modules[[1]]
  emptyModule <- minimalTestProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    extendInitialConditionsBB(icBB, spatialStructureModule, emptyModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditionsBB should handle wrong type of initialConditionsBuildingBlock", {
  simulation <- getSimulation()
  spatialStructureModule <- simulation$configuration$modules[[1]]
  moleculesModule <- simulation$configuration$modules[[1]]

  # I am not sure if this should throw an error on the R side, as there is no validation
  # but the underlying .NET code might throw.
  expect_error(
    extendInitialConditionsBB(testPVBB, spatialStructureModule, moleculesModule)
  )
})

# setParameterValuesBB tests

test_that("setParameterValuesBB throws an error if the building block is not of type Parameter Values", {
  icBB <- getFreshICBB()
  expect_error(
    setParameterValuesBB(icBB, "Path", 1, units = ""),
    regexp = "Parameter Values"
  )
})

test_that("setParameterValuesBB throws an error if quantityPaths and quantityValues have different lengths", {
  pvBB <- getFreshPVBB()
  expect_error(
    setParameterValuesBB(
      pvBB,
      quantityPaths = c("Path1", "Path2"),
      quantityValues = c(1),
      units = ""
    ),
    regexp = "The length of quantityPaths should be equal to the length "
  )
})

test_that("setParameterValuesBB sets a single entry with explicit dimensions", {
  pvBB <- getFreshPVBB()
  quantityPath <- "Organism|Liver|Volume"
  quantityValue <- 1.5

  setParameterValuesBB(
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

test_that("setParameterValuesBB derives dimensions from units when dimensions is NULL", {
  pvBB <- getFreshPVBB()
  quantityPath <- "Organism|Liver|Volume"
  quantityValue <- 1.5

  setParameterValuesBB(
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

test_that("setParameterValuesBB sets multiple entries correctly", {
  pvBB <- getFreshPVBB()
  quantityPaths <- c(
    "Organism|Liver|Volume",
    "Organism|Kidney|Volume"
  )
  quantityValues <- c(1.5, 0.3)

  setParameterValuesBB(
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

test_that("setParameterValuesBB overwrites the dimension of an existing entry", {
  pvBB <- getFreshPVBB()
  quantityPaths <- c(
    "Organism|Liver|Volume"
  )
  quantityValues <- c(1.5)

  setParameterValuesBB(
    pvBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    units = "l",
    dimensions = ospDimensions$Volume
  )

  setParameterValuesBB(
    pvBB,
    quantityPaths = "Organism|Liver|Volume",
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

test_that("setParameterValuesBB correctly updates an existing entry when the values are provided in non-base units", {
  pvBB <- getFreshPVBB()
  quantityPaths <- c(
    "Organism|Heart|Volume"
  )

  setParameterValuesBB(
    pvBB,
    quantityPaths = quantityPaths,
    quantityValues = 1,
    units = "l",
    dimensions = ospDimensions$Volume
  )

  # Now set the value in different units, but with the same dimensions. The value should be converted to the base unit and overwrite the existing value.
  setParameterValuesBB(
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

# deleteParameterValuesBB tests

test_that("deleteParameterValuesBB throws an error if the building block is not of type Parameter Values", {
  icBB <- getFreshICBB()
  expect_error(
    deleteParameterValuesBB(icBB, "Path"),
    regexp = "Parameter Values"
  )
})

test_that("deleteParameterValuesBB removes existing entries correctly", {
  pvBB <- getFreshPVBB()
  # The PV BB in this simulation is empty, so we have to add some entries first
  setParameterValuesBB(
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
  deleteParameterValuesBB(pvBB, pathToDelete)

  dfAfter <- parameterValuesBBToDataFrame(pvBB)
  expect_false(any(
    paste0(dfAfter$`Container Path`, "|", dfAfter$`Parameter Name`) ==
      pathToDelete
  ))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 1)
})

test_that("deleteParameterValuesBB removes multiple entries correctly", {
  pvBB <- getFreshPVBB()
  # The PV BB in this simulation is empty, so we have to add some entries first
  setParameterValuesBB(
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

  deleteParameterValuesBB(pvBB, pathsToDelete)

  dfAfter <- parameterValuesBBToDataFrame(pvBB)
  currentPaths <- paste0(
    dfAfter$`Container Path`,
    "|",
    dfAfter$`Parameter Name`
  )

  expect_false(any(currentPaths %in% pathsToDelete))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 2)
})

test_that("deleteParameterValuesBB ignores paths that do not exist", {
  pvBB <- getFreshPVBB()
  # The PV BB in this simulation is empty, so we have to add some entries first
  setParameterValuesBB(
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

  expect_silent(deleteParameterValuesBB(pvBB, nonExistentPath))

  dfAfter <- parameterValuesBBToDataFrame(pvBB)
  expect_equal(dfAfter, dfBefore)
})
