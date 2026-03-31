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
  # Setup fresh simulation and modules for this test
  simulation <- getSimulation()
  icBB <- simulation$configuration$modules[[1]]$getInitialConditionsBBs()[[1]]
  spatialStructureModule <- thyroidModule <- loadModuleFromPKML(system.file(
    "extdata",
    "Thyroid.pkml",
    package = "ospsuite"
  ))
  moleculesModule <- simulation$configuration$modules[[1]]

  newPaths <- extendInitialConditions(
    initialConditionsBuildingBlock = icBB,
    spatialStructureModule = spatialStructureModule,
    moleculesModule = moleculesModule
  )

  paths_after <- getAllQuantityPathsIn(icBB)

  # Check that new paths were added
  expect_gt(length(newPaths), 0)
  expect_true(all(newPaths %in% paths_after))

  # Check that new paths for the new molecule are present
  expect_true(any(grepl("new-molecule", newPaths, fixed = TRUE)))

  # Check that paths for existing molecules were not added again
  existing_paths_in_newPaths <- newPaths[newPaths %in% paths_before]
  expect_equal(length(existing_paths_in_newPaths), 0)
})

test_that("extendInitialConditions extends only with specified molecules", {
  # Setup fresh simulation and modules for this test
  simulation <- getSimulation()
  icBB <- simulation$configuration$modules[[1]]$getInitialConditionsBBs()[[1]]
  spatialStructureModule <- simulation$configuration$modules[[1]]
  moleculesModule <- simulation$configuration$modules[[1]]

  # Add two molecules but only extend one
  mol1 <- createMolecule(
    name = "mol-to-extend",
    templateMoleculeName = "Aciclovir"
  )
  mol2 <- createMolecule(
    name = "mol-not-to-extend",
    templateMoleculeName = "Aciclovir"
  )
  moleculesModule$add(mol1)
  moleculesModule$add(mol2)

  newPaths <- extendInitialConditions(
    icBB,
    spatialStructureModule,
    moleculesModule,
    moleculeNames = "mol-to-extend"
  )

  # Check that new paths for "mol-to-extend" are present
  expect_true(any(grepl("mol-to-extend", newPaths, fixed = TRUE)))
  # Check that new paths for "mol-not-to-extend" are NOT present
  expect_false(any(grepl("mol-not-to-extend", newPaths, fixed = TRUE)))
})

test_that("extendInitialConditions throws error for wrong spatial structure module", {
  icBB <- getFreshICBB()
  moleculesModule <- getSimulation()$configuration$modules[[1]]
  # A module without a spatial structure BB is needed.
  # Let's create an empty module.
  emptyModule <- MoBiModule$new("empty")

  expect_error(
    extendInitialConditions(icBB, emptyModule, moleculesModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditions throws error for wrong molecules module", {
  icBB <- getFreshICBB()
  spatialStructureModule <- getSimulation()$configuration$modules[[1]]
  emptyModule <- MoBiModule$new("empty")

  expect_error(
    extendInitialConditions(icBB, spatialStructureModule, emptyModule),
    "The provided modules do not contain the required building blocks"
  )
})

test_that("extendInitialConditions should handle wrong type of initialConditionsBuildingBlock", {
  pvBB <- getPVBB()
  simulation <- getSimulation()
  spatialStructureModule <- simulation$configuration$modules[[1]]
  moleculesModule <- simulation$configuration$modules[[1]]

  # I am not sure if this should throw an error on the R side, as there is no validation
  # but the underlying .NET code might throw.
  expect_error(
    extendInitialConditions(pvBB, spatialStructureModule, moleculesModule)
  )
})
