getSimulation <- function(loadFromCache = FALSE) {
  loadSimulation(
    aciclovirSimulationPath,
    loadFromCache = loadFromCache,
    addToCache = loadFromCache
  )
}

cachedEPBB <- getSimulation()$configuration$expressionProfiles[[1]]
cachedICBB <- getSimulation()$configuration$modules[[
  1
]]$getInitialConditionsBBs()[[1]]

# expressionProfileBBToDataFrame

test_that("expressionProfileBBToDataFrame returns a list with expressionParameters and initialConditions data frames", {
  result <- expressionProfileBBToDataFrame(cachedEPBB)
  expect_named(result, c("expressionParameters", "initialConditions"))
  expect_snapshot(result$expressionParameters)
  expect_snapshot(result$initialConditions)
})

test_that("expressionProfileBBToDataFrame throws an error if the building block is not of type Expression Profiles", {
  expect_error(
    expressionProfileBBToDataFrame(cachedICBB),
    regexp = "Expression Profile"
  )
})

# createMoBiExpressionProfileBuildingBlock

test_that("createMoBiExpressionProfileBuildingBlock creates a building block", {
  expressionProfile <- createMoBiExpressionProfileBuildingBlock(
    category = "Metabolizing Enzyme",
    moleculeName = "CYP3A4",
    speciesName = "Human"
  )

  expect_true(isOfType(expressionProfile, "BuildingBlock"))
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when category is not a string", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = 123,
      moleculeName = "CYP3A4",
      speciesName = "Human"
    )
  )
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when moleculeName is not a string", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = "Metabolizing Enzyme",
      moleculeName = 123,
      speciesName = "Human"
    )
  )
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when speciesName is not a string", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = "Metabolizing Enzyme",
      moleculeName = "CYP3A4",
      speciesName = 123
    )
  )
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when category is empty", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = "",
      moleculeName = "CYP3A4",
      speciesName = "Human"
    ),
    "argument \"category\" has empty strings!."
  )
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when moleculeName is empty", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = "Metabolizing Enzyme",
      moleculeName = "",
      speciesName = "Human"
    ),
    "argument \"moleculeName\" has empty strings!."
  )
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when speciesName is not in the Species enumeration", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = "Metabolizing Enzyme",
      moleculeName = "CYP3A4",
      speciesName = "InvalidSpecies"
    )
  )
})
