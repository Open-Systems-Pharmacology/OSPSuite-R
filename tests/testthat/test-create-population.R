context("createPopulation")

# initPKSim("C:/projects/PK-Sim/src/PKSim/bin/Debug/net472")

test_that("It can create a standard dog population", {
  skip_on_os("linux") #TODO enable again as soon as createIndividual/createPopulation runs under Linux
  
  dog <- createPopulationCharacteristics(
    species = Species$Dog,
    numberOfIndividuals = 10,
    weightMin = 2,
    weightMax = 10
  )
  dogValues <- createPopulation(populationCharacteristics = dog)
  expect_equal(dogValues$population$count, 10)
})

test_that("It can create a standard human populaiton", {
  skip_on_os("linux") #TODO enable again as soon as createIndividual/createPopulation runs under Linux
  
  human <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    numberOfIndividuals = 10
  )
  human_values <- createPopulation(populationCharacteristics = human)
  expect_equal(human_values$population$count, 10)
})

test_that("It can create a standard human populaiton with weight constraints", {
  skip_on_os("linux") #TODO enable again as soon as createIndividual/createPopulation runs under Linux
  
  human <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    numberOfIndividuals = 10,
    weightMin = 20,
    weightMax = 40
  )
  human_values <- createPopulation(populationCharacteristics = human)
  expect_equal(human_values$population$count, 10)
})

test_that("It throwns an error when creating a human with population missing", {
  skip_on_os("linux") #TODO enable again as soon as createIndividual/createPopulation runs under Linux
  
  human <- createPopulationCharacteristics(
    species = Species$Human,
    numberOfIndividuals = 10,
  )
  expect_that(createPopulation(populationCharacteristics = human), throws_error())
})


test_that("It can create a standard human population  with predefined ontogenies", {
  skip_on_os("linux") #TODO enable again as soon as createIndividual/createPopulation runs under Linux
  
  moleculeOntogeny1 <- MoleculeOntogeny$new(molecule = "MyMolecule1", ontogeny = StandardOntogeny$CYP3A4)
  moleculeOntogeny2 <- MoleculeOntogeny$new(molecule = "MyMolecule2", ontogeny = StandardOntogeny$CYP2C19)

  human <- createPopulationCharacteristics(
    species = Species$Human,
    numberOfIndividuals = 10,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    moleculeOntogenies = c(moleculeOntogeny1, moleculeOntogeny2)
  )

  human_values <- createPopulation(populationCharacteristics = human)
  paths <- human_values$population$allParameterPaths

  expect_true("MyMolecule1|Ontogeny factor" %in% paths)
  expect_true("MyMolecule1|Ontogeny factor GI" %in% paths)

  expect_true("MyMolecule2|Ontogeny factor" %in% paths)
  expect_true("MyMolecule2|Ontogeny factor GI" %in% paths)
})
