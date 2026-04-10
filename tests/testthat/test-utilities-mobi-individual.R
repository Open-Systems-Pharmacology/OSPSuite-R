# createIndividualBuildingBlock

test_that("createIndividualBuildingBlock creates a building block for a non-human species", {
  individual <- createIndividualBuildingBlock(
    species = Species$Beagle
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createIndividualBuildingBlock creates a building block for a human individual", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    age = 30
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createIndividualBuildingBlock throws an error when species is Human and wrong population is provided", {
  expect_error(
    createIndividualBuildingBlock(
      species = Species$Human,
      population = "NAN"
    ),
    messages$errorWrongPopulation(Species$Human, "NAN")
  )
})

test_that("createIndividualBuildingBlock respects a specified seed", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    seed = 42
  )
  expect_true(isOfType(individual, "BuildingBlock"))
})

# setMoBiIndividualParameters

test_that("setMoBiIndividualParameters sets parameters on a building block", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )
  expect_no_error(
    setMoBiIndividualParameters(
      individual,
      quantityPaths = c("Organism|Age", "Organism|BMI"),
      quantityValues = c(30, 73)
    )
  )
})

test_that("setMoBiIndividualParameters returns the building block invisibly", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )

  result <- setMoBiIndividualParameters(
    individual,
    quantityPaths = "Organism|Age",
    quantityValues = 25
  )

  expect_identical(result, individual)
})

test_that("setMoBiIndividualParameters throws an error when quantityPaths and quantityValues have different lengths", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )

  expect_error(
    setMoBiIndividualParameters(
      individual,
      quantityPaths = c("Organism|Age", "Organism|BMI"),
      quantityValues = 30
    )
  )
})

test_that("setMoBiIndividualParameters throws an error when individualBuildingBlock is not a BuildingBlock", {
  expect_error(
    setMoBiIndividualParameters(
      "not a building block",
      quantityPaths = "Organism|Age",
      quantityValues = 30
    )
  )
})
