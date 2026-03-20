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
