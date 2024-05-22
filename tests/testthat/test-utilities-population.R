skip()
skip_on_os("linux") # TODO enable again as soon as createIndividual/createPopulation runs under Linux
skip_on_ci()

# createPopulation

test_that("It can create a standard dog population", {
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
  human <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    numberOfIndividuals = 10,
    seed = 1234
  )
  human_values <- createPopulation(populationCharacteristics = human)
  expect_equal(human_values$population$count, 10)
  expect_equal(human_values$seed, human$seed)
})

test_that("It returns the random seed used if not specified", {
  human <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$BlackAmerican_NHANES_1997,
    numberOfIndividuals = 10,
  )
  human_values <- createPopulation(populationCharacteristics = human)
  expect_gt(human_values$seed, 0)
})

test_that("It can create a standard human populaiton with weight constraints", {
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
  human <- createPopulationCharacteristics(
    species = Species$Human,
    numberOfIndividuals = 10,
  )
  expect_error(createPopulation(populationCharacteristics = human))
})


test_that("It can create a standard human population  with predefined ontogenies", {
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


# loadPopulation

test_that("It can load a valid csv population file", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(populationFileName)
  expect_true(!is.null(population))
})

test_that("It throws an exception when loading an invalid population file", {
  populationFileName <- getTestDataFilePath("junk.csv")
  expect_error(loadPopulation(populationFileName))
})


# splitPopulationFile
test_that("It can split a valid csv file to split files", {
  populationFileName <- getTestDataFilePath("pop.csv")
  splitFiles <- splitPopulationFile(populationFileName, 3, tempdir(), "SplitFile")
  expect_equal(length(splitFiles), 3)
})


# populationToDataFrame
test_that("It can convert a population to data frame", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(populationFileName)
  df <- populationToDataFrame(population)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10)
})


# populationToTibble
test_that("It can convert a population to tibble data frame", {
  populationFileName <- getTestDataFilePath("pop.csv")
  population <- loadPopulation(populationFileName)
  df <- populationToTibble(population)
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 10)
})
