# createMoBiIndividualBuildingBlock

test_that("createMoBiIndividualBuildingBlock creates a building block for a non-human species", {
  individual <- createMoBiIndividualBuildingBlock(
    species = Species$Beagle
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createMoBiIndividualBuildingBlock creates a building block for a human individual", {
  individual <- createMoBiIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    age = 30
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createMoBiIndividualBuildingBlock creates a building block for a human individual with only species and population specified", {
  individual <- createMoBiIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createMoBiIndividualBuildingBlock throws an error when species is Human and no population is provided", {
  expect_error(
    createMoBiIndividualBuildingBlock(
      species = Species$Human
    ),
    messages$errorWrongPopulation(Species$Human, NULL)
  )
})

test_that("createMoBiIndividualBuildingBlock throws an error when species is Human and wrong population is provided", {
  expect_error(
    createMoBiIndividualBuildingBlock(
      species = Species$Human,
      population = "NAN"
    ),
    messages$errorWrongPopulation(Species$Human, "NAN")
  )
})

test_that("createMoBiIndividualBuildingBlock respects a specified seed", {
  individual <- createMoBiIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    seed = 42
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})
