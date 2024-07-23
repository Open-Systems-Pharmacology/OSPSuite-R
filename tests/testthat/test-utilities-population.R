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

test_that("simulationResultsToDataFrame with population", {

  # If no unit is specified, the default units are used. For "height" it is "dm",
  # for "weight" it is "kg", for "age" it is "year(s)".
  populationCharacteristics <- createPopulationCharacteristics(
    species = Species$Human,
    population = HumanPopulation$Asian_Tanaka_1996,
    numberOfIndividuals = 50,
    proportionOfFemales = 50,
    weightMin = 30,
    weightMax = 98,
    weightUnit = "kg",
    heightMin = NULL,
    heightMax = NULL,
    ageMin = 0,
    ageMax = 80,
    ageUnit = "year(s)"
  )

  # Create population from population characteristics
  result <- createPopulation(populationCharacteristics = populationCharacteristics)
  myPopulation <- result$population

  # Load simulation
  simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
  sim <- loadSimulation(simFilePath)

  populationResults <- runSimulations(
    simulations = sim,
    population = myPopulation
  )[[1]]

  df1 <- simulationResultsToDataFrame(populationResults)

  expect_equal(dim(df1), c(24550L, 9L))
  expect_equal(
    unique(df1$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(min(df1$IndividualId), 0)
  expect_equal(max(df1$IndividualId), 49)
  expect_equal(unique(df1$unit), .encodeUnit("µmol/l"))
  expect_equal(unique(df1$dimension), "Concentration (molar)")
  expect_equal(unique(df1$TimeUnit), "min")

  df2 <- simulationResultsToDataFrame(populationResults, individualIds = c(1, 4, 5))

  expect_equal(dim(df2), c(1473L, 9L))
  expect_equal(
    unique(df2$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(min(df2$IndividualId), 1)
  expect_equal(max(df2$IndividualId), 5)
  expect_equal(unique(df2$unit), .encodeUnit("µmol/l"))
  expect_equal(unique(df2$dimension), "Concentration (molar)")
  expect_equal(unique(df2$TimeUnit), "min")
})
