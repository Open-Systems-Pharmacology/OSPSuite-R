context("createIndividual")

# initPKSim("C:/projects/PK-Sim/src/PKSim/bin/Debug/net472")

test_that("It can create a standard dog for a given bodyweight", {
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

  dog <- createIndividualCharacteristics(
    species = Species$Dog,
    weight = 10
  )
  dogValues <- createIndividual(individualCharacteristics = dog)
  expect_false(is.null((dogValues)))
})

test_that("It can create a standard human for a given bodyweight", {
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

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
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    weight = 60,
    gender = Gender$Female
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
})

test_that("It returns the given seed if passed as parameter", {
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

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
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

  human <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
  )

  human_values <- createIndividual(individualCharacteristics = human)
  expect_false(is.null((human_values)))
  expect_gt(human_values$seed, 0)
})

test_that("It throwns an error when creating a human with population missing", {
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

  human <- createIndividualCharacteristics(
    species = Species$Human,
    weight = 60,
    age = 15,
    gender = Gender$Female
  )
  expect_that(createIndividual(individualCharacteristics = human), throws_error())
})

test_that("It can create reating a human with weight missing", {
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

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
  skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux

  moleculeOntogeny1 <- MoleculeOntogeny$new(molecule = "MyMolecule1", ontogeny = StandardOntogeny$CYP3A4)
  moleculeOntogeny2 <- MoleculeOntogeny$new(molecule = "MyMolecule2", ontogeny = StandardOntogeny$CYP2C19)

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
