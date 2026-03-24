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
    "The `category` argument must not be an empty string."
  )
})

test_that("createMoBiExpressionProfileBuildingBlock throws an error when moleculeName is empty", {
  expect_error(
    createMoBiExpressionProfileBuildingBlock(
      category = "Metabolizing Enzyme",
      moleculeName = "",
      speciesName = "Human"
    ),
    "The `moleculeName` argument must not be an empty string."
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
