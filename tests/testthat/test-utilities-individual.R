# createIndividualCharacteristics

test_that("It does not throw an error when species is not human and no population is provided", {
  expect_no_error(
    individualCharacteristics <- createIndividualCharacteristics(
      species = Species$Beagle,
      height = NULL
    )
  )
})

test_that("It throws an error when species is Human and no population is provided", {
  expect_error(
    individualCharacteristics <- createIndividualCharacteristics(
      species = Species$Human,
      height = NULL
    ),
    messages$errorWrongPopulation(Species$Human, NULL)
  )
})

test_that("It throws an error when species is Human and wrong is provided", {
  expect_error(
    individualCharacteristics <- createIndividualCharacteristics(
      species = Species$Human,
      population = "NAN",
      height = NULL
    ),
    messages$errorWrongPopulation(Species$Human, "NAN")
  )
})

# createIndividual

test_that("It can create a standard dog for a given bodyweight", {
  dog <- createIndividualCharacteristics(
    species = Species$Dog,
    weight = 10
  )
  dogValues <- createIndividual(individualCharacteristics = dog)
  expect_false(is.null((dogValues)))
})

test_that("It can create a standard human for a given bodyweight", {
  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    weight = 60,
    age = 15,
    gender = Gender$Female
  )
  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
})

test_that("It does not throw an error when creating a human with age missing", {
  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    weight = 60,
    gender = Gender$Female
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
})

test_that("It creates a human individual when age, weight, height, or gestational age are `NA`", {
  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    weight = NA,
    height = NA,
    age = NA,
    gestationalAge = NA,
    gender = Gender$Female
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
})

test_that("It returns the given seed if passed as parameter", {
  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    seed = 1234
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
  expect_equal(human$seed, human_values$seed)
})

test_that("It sets a random seed if not specified", {
  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
  expect_gt(human_values$seed, 0)
})

test_that("It can create reating a human with weight missing", {
  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    age = 15,
    gender = Gender$Female
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
})

test_that("It can create a standard human for a given bodyweight with predefined ontogenies", {
  moleculeOntogeny1 <- MoleculeOntogeny$new(
    molecule = "MyMolecule1",
    ontogeny = StandardOntogeny$CYP3A4
  )
  moleculeOntogeny2 <- MoleculeOntogeny$new(
    molecule = "MyMolecule2",
    ontogeny = StandardOntogeny$CYP2C19
  )

  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    weight = 60,
    age = 15,
    gender = Gender$Female,
    moleculeOntogenies = c(moleculeOntogeny1, moleculeOntogeny2)
  )

  human_values <- createIndividual(individualCharacteristics = human)
  paths <- human_values$distributedParameters$paths

  expect_true("MyMolecule1|Ontogeny factor" %in% paths)
  expect_true("MyMolecule1|Ontogeny factor GI" %in% paths)

  expect_true("MyMolecule2|Ontogeny factor" %in% paths)
  expect_true("MyMolecule2|Ontogeny factor GI" %in% paths)
})
