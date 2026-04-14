# createIndividualBuildingBlock

test_that("createIndividualBuildingBlock creates a building block for a non-human species", {
  individual <- createIndividualBuildingBlock(
    species = Species$Beagle
  )

  expect_true(isOfType(individual, "BuildingBlock"))
  expect_equal(individual$type, BuildingBlockTypes$Individual)
  expect_equal(individual$name, Species$Beagle)
})

test_that("createIndividualBuildingBlock creates a building block with the provided name", {
  individual <- createIndividualBuildingBlock(
    name = "Test Individual",
    species = Species$Beagle
  )

  expect_equal(individual$name, "Test Individual")
})

test_that("createIndividualBuildingBlock creates a building block for a human individual", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002,
    gender = Gender$Male,
    weight = 73,
    age = 30,
    seed = 42
  )

  expect_true(isOfType(individual, "BuildingBlock"))
})

test_that("createIndividualBuildingBlock throws an error when species is Human and no or wrong population is provided", {
  expect_error(
    createIndividualBuildingBlock(
      species = Species$Human
    ),
    messages$errorWrongPopulation(Species$Human, NULL)
  )

  expect_error(
    createIndividualBuildingBlock(
      species = Species$Human,
      population = "NAN"
    ),
    messages$errorWrongPopulation(Species$Human, "NAN")
  )
})

# individualsBBToDataFrame
test_that("individualsBBToDataFrame returns a data frame with expected columns", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )

  df <- individualsBBToDataFrame(individual)

  expect_true(is.data.frame(df))
  expect_equal(
    colnames(df),
    c(
      "Container Path",
      "Parameter Name",
      "Value",
      "Unit",
      "Value Origin"
    )
  )
})

# setParameterValuesInIndividualBB

test_that("setParameterValuesInIndividualBB sets parameters on a building block", {
  individual <- createIndividualBuildingBlock(
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )
  setParameterValuesInIndividualBB(
    individual,
    quantityPaths = c("Organism|Age", "Organism|BMI"),
    quantityValues = c(30, 73),
    units = c("year(s)", "kg/m²")
  )

  df <- individualsBBToDataFrame(individual)
  entries <- df[
    (df$`Container Path` == "Organism" &
      df$`Parameter Name` == "BMI") |
      (df$`Container Path` == "Organism" &
        df$`Parameter Name` == "Age"),
  ]
  # Returned values are converted to the reference unit, which is year for age and kg/m² for BMI. Therefore, the value for BMI is converted from 73 kg/m² to 0.73 kg/cm².
  expect_equal(entries$Value, c(0.73, 30))
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
    quantityValues = 25,
    units = "year(s)"
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
      quantityValues = 30,
      units = c("year(s)", "kg/m²")
    ),
    "The length of quantityPaths should be equal to the length of quantityValues"
  )
})

test_that("setParameterValuesInIndividualBB throws an error when individualBuildingBlock is not a BuildingBlock", {
  expect_error(
    setParameterValuesInIndividualBB(
      "not a building block",
      quantityPaths = "Organism|Age",
      quantityValues = 30,
      units = "year(s)"
    ),
    "but expected <BuildingBlock>!"
  )
})

# throws an error when units are not compatible with the reference dimensions
test_that("setParameterValuesInIndividualBB throws an error if one of the provided units is not valid", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )
  expect_error(
    setParameterValuesInIndividualBB(
      individual,
      quantityPaths = c("Organism|Age", "Organism|BMI"),
      quantityValues = c(30, 73),
      units = c("year(s)", "min")
    ),
    regexp = "Unit \"min\" is not supported by"
  )
})


# throws an error when trying to set a parameter that is not in the bb
test_that("setParameterValuesInIndividualBB throws an error if trying to set a parameter that is not in the building block", {
  individual <- createIndividualBuildingBlock(
    name = "testName",
    species = Species$Human,
    population = HumanPopulation$European_ICRP_2002
  )
  expect_error(
    setParameterValuesInIndividualBB(
      individual,
      quantityPaths = c(
        "Organism|NonExistingParameter",
        "Organism|Age",
        "SecondNonExistingParameter"
      ),
      quantityValues = c(30),
      units = c("year(s)")
    ),
    regexp = "are not present in the building block: Organism|NonExistingParameter, SecondNonExistingParameter"
  )
})
