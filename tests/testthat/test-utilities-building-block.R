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

testProject <- loadMoBiProject(system.file(
  "extdata",
  "TH_QST_Platform.mbp3",
  package = "ospsuite"
))
minimalTestProject <- loadMoBiProject(getTestDataFilePath("Test_Project.mbp3"))

# Test initialConditionsToDataFrame with no paths
# Test initialConditionsToDataFrame with one path
# Test initialConditionsToDataFrame with multiple paths
test_that("initialConditionsToDataFrame returns a data frame with the expected columns", {
  icBB <- getFreshICBB()
  df <- initialConditionsToDataFrame(icBB)
  expect_snapshot(df)
})

test_that("initialConditionsToDataFrame throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    initialConditionsToDataFrame(testPVBB),
    regexp = "Initial Conditions"
  )
})


# test for wrong type of building block
test_that("setInitialConditions throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    setInitialConditions(testPVBB, "Path", 1),
    regexp = "Initial Conditions"
  )
})

# test for differnt lengths of quantityPaths and quantityValues
test_that("setInitialConditions throws an error if quantityPaths and quantityValues have different lengths", {
  icBB <- getFreshICBB()
  expect_error(
    setInitialConditions(
      icBB,
      quantityPaths = c("Path1", "Path2"),
      quantityValues = c(1)
    ),
    regexp = "The length of quantityPaths should be equal to the length "
  )
})


test_that("setInitialConditions updates one entry correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- "Organism|Brain|Intracellular|Aciclovir"
  quantityValues <- 10

  # Set initial conditions
  setInitialConditions(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 1,
    isPresent = TRUE,
    negativeValuesAllowed = FALSE
  )

  df <- initialConditionsToDataFrame(icBB)

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
test_that("setInitialConditions updates multiple entries correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- c(
    "Organism|Brain|Intracellular|Aciclovir",
    "Organism|Kidney|Intracellular|Aciclovir"
  )
  quantityValues <- c(10, 20)

  # Set initial conditions
  setInitialConditions(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 2,
    isPresent = TRUE,
    negativeValuesAllowed = TRUE
  )

  df <- initialConditionsToDataFrame(icBB)

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
test_that("setInitialConditions adds new entries correctly", {
  icBB <- getFreshICBB()
  quantityPaths <- c(
    "Organism|Intracellular|Aciclovir",
    "Organism|Heart|New|Aciclovir"
  )
  quantityValues <- c(30, 40)

  # Set initial conditions
  setInitialConditions(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 1,
    isPresent = TRUE,
    negativeValuesAllowed = FALSE
  )

  df <- initialConditionsToDataFrame(icBB)
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

test_that("deleteInitialConditions throws an error if the building block is not of type Initial Conditions", {
  expect_error(
    deleteInitialConditions(testPVBB, "Organism|Brain|Intracellular|Aciclovir"),
    regexp = "Initial Conditions"
  )
})

test_that("deleteInitialConditions removes existing entries correctly", {
  icBB <- getFreshICBB()
  # Get initial count
  dfBefore <- initialConditionsToDataFrame(icBB)
  pathToDelete <- "Organism|Brain|Intracellular|Aciclovir"

  # Ensure the path exists before deletion
  expect_true(any(
    paste0(dfBefore$`Container Path`, "|", dfBefore$`Molecule Name`) ==
      pathToDelete
  ))

  # Delete the entry
  deleteInitialConditions(icBB, pathToDelete)

  # Check after deletion
  dfAfter <- initialConditionsToDataFrame(icBB)
  expect_false(any(
    paste0(dfAfter$`Container Path`, "|", dfAfter$`Molecule Name`) ==
      pathToDelete
  ))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 1)
})

test_that("deleteInitialConditions removes multiple entries correctly", {
  icBB <- getFreshICBB()
  dfBefore <- initialConditionsToDataFrame(icBB)
  pathsToDelete <- c(
    "Organism|Kidney|Intracellular|Aciclovir",
    "Organism|Muscle|Intracellular|Aciclovir"
  )

  deleteInitialConditions(icBB, pathsToDelete)

  dfAfter <- initialConditionsToDataFrame(icBB)
  currentPaths <- paste0(dfAfter$`Container Path`, "|", dfAfter$`Molecule Name`)

  expect_false(any(currentPaths %in% pathsToDelete))
  expect_equal(nrow(dfAfter), nrow(dfBefore) - 2)
})

test_that("deleteInitialConditions ignores paths that do not exist", {
  icBB <- getFreshICBB()
  dfBefore <- initialConditionsToDataFrame(icBB)
  nonExistentPath <- "Non|Existent|Path|Molecule"

  # Should not throw an error
  expect_silent(deleteInitialConditions(icBB, nonExistentPath))

  dfAfter <- initialConditionsToDataFrame(icBB)
  expect_equal(dfAfter, dfBefore)
})

# extendInitialConditions
test_that("extendInitialConditions extends with all molecules if moleculeNames is NULL", {
  module <- testProject$getModules("Thyroid_QST")[[1]]
  icBB <- module$getInitialConditionsBBs()[[1]]
  spatialStructureModule <- thyroidModule <- loadModuleFromPKML(system.file(
    "extdata",
    "Thyroid.pkml",
    package = "ospsuite"
  ))

  newPaths <- extendInitialConditions(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = spatialStructureModule,
    moleculesModule = module
  )

  icBB_df <- initialConditionsToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditions does not add new entries for existing molecules and compartments", {
  module <- testProject$getModules("Thyroid_QST")[[1]]
  icBB <- module$getInitialConditionsBBs()[[1]]

  newPaths <- extendInitialConditions(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = module,
    moleculesModule = module,
    moleculeNames = c("T3", "T4")
  )

  # New entries are still added , specifically for the "EndogenousIgG" compartment. This is expected.
  icBB_df <- initialConditionsToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditions extends only with specified molecules", {
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

  newPaths <- extendInitialConditions(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = spatialStructureModule,
    moleculesModule = module,
    moleculeNames = c("T3", "T4")
  )

  icBB_df <- initialConditionsToDataFrame(icBB)
  # Select only the new paths
  newPaths_df <- icBB_df[
    paste0(icBB_df$`Container Path`, "|", icBB_df$`Molecule Name`) %in%
      newPaths,
  ]

  expect_snapshot(newPaths_df)
})

test_that("extendInitialConditions throws error for wrong spatial structure module", {
  icBB <- getFreshICBB()
  moleculesModule <- getSimulation()$configuration$modules[[1]]
  # A module without a spatial structure BB is needed.
  emptyModule <- minimalTestProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    extendInitialConditions(icBB, emptyModule, moleculesModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditions throws error for wrong molecules module", {
  icBB <- getFreshICBB()
  spatialStructureModule <- getSimulation()$configuration$modules[[1]]
  emptyModule <- minimalTestProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    extendInitialConditions(icBB, spatialStructureModule, emptyModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditions should handle wrong type of initialConditionsBuildingBlock", {
  simulation <- getSimulation()
  spatialStructureModule <- simulation$configuration$modules[[1]]
  moleculesModule <- simulation$configuration$modules[[1]]

  # I am not sure if this should throw an error on the R side, as there is no validation
  # but the underlying .NET code might throw.
  expect_error(
    extendInitialConditions(testPVBB, spatialStructureModule, moleculesModule)
  )
})
