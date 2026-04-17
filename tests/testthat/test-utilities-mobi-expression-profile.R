sim <- loadSimulation(
  aciclovirSimulationPath,
  loadFromCache = TRUE
)

cachedEPBB <- sim$configuration$expressionProfiles[[1]]
cachedICBB <- sim$configuration$modules[[
  1
]]$getInitialConditionsBBs()[[1]]

# expressionProfileBBToDataFrame

test_that("expressionProfileBBToDataFrame returns a list with expressionParameters and initialConditions data frames", {
  result <- expressionProfileBBToDataFrame(cachedEPBB)
  expect_named(result, c("expressionParameters", "initialConditions"))
  expect_snapshot(result$expressionParameters)
  expect_snapshot(result$initialConditions)
})

test_that("expressionProfileBBToDataFrame accepts a list of building blocks and appends data frames", {
  ep1 <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )
  ep2 <- createExpressionProfileBuildingBlock(
    type = "Transport Protein",
    moleculeName = "ABCB1",
    speciesName = "Human"
  )

  resultSingle1 <- expressionProfileBBToDataFrame(ep1)
  resultSingle2 <- expressionProfileBBToDataFrame(ep2)
  resultList <- expressionProfileBBToDataFrame(list(ep1, ep2))

  expect_equal(
    nrow(resultList$expressionParameters),
    nrow(resultSingle1$expressionParameters) +
      nrow(resultSingle2$expressionParameters)
  )
  expect_equal(
    nrow(resultList$initialConditions),
    nrow(resultSingle1$initialConditions) +
      nrow(resultSingle2$initialConditions)
  )
  expect_equal(
    colnames(resultList$expressionParameters),
    colnames(resultSingle1$expressionParameters)
  )
  expect_equal(
    colnames(resultList$initialConditions),
    colnames(resultSingle1$initialConditions)
  )
})

test_that("expressionProfileBBToDataFrame throws an error if the building block is not of type Expression Profiles", {
  expect_error(
    expressionProfileBBToDataFrame(cachedICBB),
    regexp = "Expression Profile"
  )
})

# createExpressionProfileBuildingBlock

test_that("createExpressionProfileBuildingBlock creates a building block", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  expect_true(isOfType(expressionProfile, "BuildingBlock"))
  expect_snapshot(expressionProfile)
})

# wrong type
test_that("createExpressionProfileBuildingBlock throws an error when protein type is not supported", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "InvalidType",
      moleculeName = "CYP3A4",
      speciesName = "Human"
    ),
    regexp = "is not a valid value in `ExpressionProfileCategories`"
  )
})

# wrong moleculeName

test_that("createExpressionProfileBuildingBlock throws an error when moleculeName is not a string", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "Metabolizing Enzyme",
      moleculeName = 123,
      speciesName = "Human"
    ),
    regexp = "argument \"moleculeName\" is of type <numeric>"
  )
})

test_that("createExpressionProfileBuildingBlock throws an error when speciesName is not a string", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "Metabolizing Enzyme",
      moleculeName = "CYP3A4",
      speciesName = 123
    ),
    regexp = "is not a valid value in `Species`"
  )
})

test_that("createExpressionProfileBuildingBlock throws an error when type is empty", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "",
      moleculeName = "CYP3A4",
      speciesName = "Human"
    ),
    "is not a valid value in "
  )
})

test_that("createExpressionProfileBuildingBlock throws an error when moleculeName is empty", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "Metabolizing Enzyme",
      moleculeName = "",
      speciesName = "Human"
    ),
    "argument \"moleculeName\" has empty strings!."
  )
})

test_that("createExpressionProfileBuildingBlock throws an error when speciesName is empty", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "Metabolizing Enzyme",
      moleculeName = "CYP3A4",
      speciesName = ""
    ),
    regexp = "is not a valid value in `Species`"
  )
})

test_that("createExpressionProfileBuildingBlock throws an error when speciesName is not in the Species enumeration", {
  expect_error(
    createExpressionProfileBuildingBlock(
      type = "Metabolizing Enzyme",
      moleculeName = "CYP3A4",
      speciesName = "InvalidSpecies"
    ),
    regexp = "is not a valid value in `Species`"
  )
})

# setExpressionProfileParameters

test_that("setExpressionProfileParameters successfully applies new values", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  # Get existing parameters to find a valid quantity path
  dfBefore <- expressionProfileBBToDataFrame(expressionProfile)
  epDf <- dfBefore$expressionParameters
  containerPath <- epDf$`Container Path`[1]
  paramName <- epDf$`Parameter Name`[1]
  quantityPath <- paste(containerPath, paramName, sep = "|")
  displayUnit <- epDf$Unit[1]

  setExpressionProfileParameters(
    expressionProfile,
    quantityPaths = quantityPath,
    quantityValues = 42,
    units = displayUnit
  )

  dfAfter <- expressionProfileBBToDataFrame(expressionProfile)
  entry <- dfAfter$expressionParameters[
    dfAfter$expressionParameters$`Container Path` == containerPath &
      dfAfter$expressionParameters$`Parameter Name` == paramName,
  ]
  expect_equal(entry$Value, 42)
})

test_that("setExpressionProfileParameters applies new values in non-base units", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  # Get existing parameters and find one with a non-empty unit (i.e., a dimensional quantity)
  dfBefore <- expressionProfileBBToDataFrame(expressionProfile)
  epDf <- dfBefore$expressionParameters
  containerPath <- "Organism|VenousBlood|Plasma|CYP3A4"
  paramName <- "Initial concentration"
  quantityPath <- paste(containerPath, paramName, sep = "|")
  targetUnit <- "pmol/l"
  dimension <- getDimensionForUnit(targetUnit)
  baseUnit <- getBaseUnit(dimension)

  # Set the value in base units
  valueInTargetUnit <- 100
  setExpressionProfileParameters(
    expressionProfile,
    quantityPaths = quantityPath,
    quantityValues = valueInTargetUnit,
    units = targetUnit
  )

  # The data frame returns values in the display unit
  dfAfter <- expressionProfileBBToDataFrame(expressionProfile)
  entry <- dfAfter$expressionParameters[
    dfAfter$expressionParameters$`Container Path` == containerPath &
      dfAfter$expressionParameters$`Parameter Name` == paramName,
  ]
  expectedValue <- toBaseUnit(dimension, valueInTargetUnit, targetUnit)
  expect_equal(entry$Value, expectedValue)
})

test_that("setExpressionProfileParameters throws an error when trying to set a value for a parameter that is not in the building block", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  expect_error(
    setExpressionProfileParameters(
      expressionProfile,
      quantityPaths = "NonExisting|Parameter",
      quantityValues = 1.0,
      units = ""
    ),
    regexp = "Cannot add new entries to the Expression Profile."
  )
})

test_that("setExpressionProfileParameters throws an error when expressionProfileBuildingBlock is not a BuildingBlock", {
  expect_error(
    setExpressionProfileParameters(
      expressionProfileBuildingBlock = "notABuildingBlock",
      quantityPaths = "path",
      quantityValues = 1.0
    ),
    regexp = "is of type"
  )
})

test_that("setExpressionProfileParameters throws an error when quantityPaths is not a string", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  expect_error(
    setExpressionProfileParameters(
      expressionProfileBuildingBlock = expressionProfile,
      quantityPaths = 123,
      quantityValues = 1.0
    ),
    regexp = "is of type"
  )
})

test_that("setExpressionProfileParameters throws an error when quantityValues is not numeric", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  expect_error(
    setExpressionProfileParameters(
      expressionProfileBuildingBlock = expressionProfile,
      quantityPaths = "path",
      quantityValues = "notNumeric"
    ),
    regexp = "is of type"
  )
})

test_that("setExpressionProfileParameters throws an error when quantityPaths and quantityValues have different lengths", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  expect_error(
    setExpressionProfileParameters(
      expressionProfileBuildingBlock = expressionProfile,
      quantityPaths = c("path1", "path2"),
      quantityValues = 1.0
    ),
    regexp = "must have the same length"
  )
})

# saveExpressionProfileToPKML tests

test_that("saveExpressionProfileToPKML writes a non-empty pkml file and returns the path invisibly", {
  filePath <- withr::local_tempfile(fileext = ".pkml")
  result <- saveExpressionProfileToPKML(cachedEPBB, filePath)

  expect_true(file.exists(filePath))
  expect_gt(file.info(filePath)$size, 0)
  expect_equal(result, .expandPath(filePath))
})

test_that("saveExpressionProfileToPKML errors when given a non-Expression-Profile BB", {
  individual <- createIndividualBuildingBlock(species = Species$Beagle)
  filePath <- withr::local_tempfile(fileext = ".pkml")
  expect_error(
    saveExpressionProfileToPKML(individual, filePath),
    regexp = "Expression Profile"
  )
})

test_that("saveExpressionProfileToPKML errors when file extension is not pkml", {
  expressionProfile <- createExpressionProfileBuildingBlock(
    type = ExpressionProfileCategories$`Metabolizing Enzyme`,
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )
  filePath <- withr::local_tempfile(fileext = ".txt")
  expect_error(
    saveExpressionProfileToPKML(expressionProfile, filePath),
    regexp = "extension 'txt', while 'pkml' was expected"
  )
})
