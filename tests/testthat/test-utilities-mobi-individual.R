# createIndividualBuildingBlock

test_that("createIndividualBuildingBlock creates a building block for a non-human species", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Beagle
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createIndividualBuildingBlock creates a building block for a human individual", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    age = 30,
    seed = 42
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createMoBiIndividualBuildingBlock throws an error when species is Human and no population is provided", {
  expect_error(
    createMoBiIndividualBuildingBlock(
      name = "testName",
      species = Species$Human
    ),
    messages$errorWrongPopulation(Species$Human, NULL)
  )
})
# setParameterValuesInIndividualBB

test_that("createMoBiIndividualBuildingBlock throws an error when species is Human and wrong population is provided", {
  expect_error(
    createMoBiIndividualBuildingBlock(
      name = "testName",
      species = Species$Human,
      population = "NAN"
    ),
    messages$errorWrongPopulation(Species$Human, "NAN")
  )
})

test_that("createMoBiIndividualBuildingBlock respects a specified seed", {
  individual <- createMoBiIndividualBuildingBlock(
test_that("setParameterValuesInIndividualBB sets parameters on a building block", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    seed = 42
  )
  expect_true(isOfType(individual, "BuildingBlock"))
})

# setMoBiIndividualParameters

test_that("setMoBiIndividualParameters sets parameters on a building block", {
  individual <- createMoBiIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )
  expect_no_error(
    setParameterValuesInIndividualBB(
      individual,
      quantityPaths = c("Organism|Age", "Organism|BMI"),
      quantityValues = c(30, 73)
    )
  )
})

test_that("setParameterValuesInIndividualBB returns the building block invisibly", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )

  result <- setParameterValuesInIndividualBB(
    individual,
    quantityPaths = "Organism|Age",
    quantityValues = 25
  )

  expect_identical(result, individual)
})

test_that("setParameterValuesInIndividualBB throws an error when quantityPaths and quantityValues have different lengths", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )

  expect_error(
    setParameterValuesInIndividualBB(
      individual,
      quantityPaths = c("Organism|Age", "Organism|BMI"),
      quantityValues = 30
    )
  )
})

test_that("setParameterValuesInIndividualBB throws an error when individualBuildingBlock is not a BuildingBlock", {
  expect_error(
    setParameterValuesInIndividualBB(
      "not a building block",
      quantityPaths = "Organism|Age",
      quantityValues = 30
    )
  )
})

# throws an error when units are not compatible with the reference dimensions

# throws an error when trying to set a parameter that is not in the bb

# correctly sets parameters when units are compatible with the reference dimensions

parameterValuesBBToDataFrame(individual)
