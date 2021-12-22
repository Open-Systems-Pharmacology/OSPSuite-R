test_that("dataCombined - both dataSet and SimulationResults provided", {
  skip_if_not_installed("R6")

  expect_true(R6::is.R6Class(DataCombined))

  # load the simulation
  sim <- loadSimulation(file.path(getwd(), "..", "data", "MinimalModel.pkml"))
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = file.path(getwd(), "..", "data", "Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = file.path(getwd(), "..", "data", "CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(file.path(getwd(), "..", "data", "ImporterConfiguration.xml"))
  )

  # with list input ----------------------------

  # created object with datasets combined
  myCombDat <- DataCombined$new()
  myCombDat$addSimulationResults(
    simResults,
    quantitiesOrPaths = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )
  myCombDat$addDataSet(dataSet)

  expect_true(R6::is.R6(myCombDat))
  expect_false(R6::is.R6Class(myCombDat))

  # checking dataframe methods
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(830L, 21L))

  # check exact values
  expect_equal(
    names(df),
    c(
      "dataType", "name", "xValues", "yValues", "yErrorValues", "xDimension",
      "xUnit", "yDimension", "yUnit", "yErrorType", "yErrorUnit", "molWeight",
      "lloq", "Source", "Sheet", "Organ", "Compartment", "Molecule",
      "Group Id", "IndividualId", "paths"
    )
  )

  # There will be NAs because there will be no paths values for observed data
  expect_equal(
    as.character(na.omit(unique(df$paths))),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )

  # with DataSet input ----------------------------

  # created object with datasets combined
  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)
  myCombDat2$addDataSet(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1267L, 21L))

  # with nonsense list inputs ----------------------------

  myCombDat3 <- DataCombined$new()
  expect_error(myCombDat3$addDataSet(list("x" = 1, "y" = 2)))

  expect_error(myCombDat3$addSimulationResults(list(simResults, simResults)))
})


test_that("dataCombined - either dataSet or SimulationResults provided", {
  skip_if_not_installed("R6")

  # load the simulation
  sim <- loadSimulation(file.path(getwd(), "..", "data", "MinimalModel.pkml"))
  simResults <- importResultsFromCSV(
    simulation = sim,
    filePaths = file.path(getwd(), "..", "data", "Stevens_2012_placebo_indiv_results.csv")
  )

  # import observed data (will return a list of DataSet objects)
  dataSet <- loadDataSetsFromExcel(
    xlsFilePath = file.path(getwd(), "..", "data", "CompiledDataSetStevens2012.xlsx"),
    importerConfiguration = DataImporterConfiguration$new(file.path(getwd(), "..", "data", "ImporterConfiguration.xml"))
  )

  # only DataSet input ----------------------------

  myCombDat <- DataCombined$new()
  myCombDat$addDataSet(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat))
  expect_false(R6::is.R6Class(myCombDat))

  # no snapshot test for this object because DataSet objects print source file location
  # this is not going to be the same on CI platforms and so the test will fail

  # checking dataframe methods
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(12L, 19L))

  expect_equal(
    names(df),
    c(
      "dataType", "name", "xValues", "yValues", "yErrorValues", "xDimension",
      "xUnit", "yDimension", "yUnit", "yErrorType", "yErrorUnit", "molWeight",
      "lloq", "Source", "Sheet", "Organ", "Compartment", "Molecule",
      "Group Id"
    )
  )

  # only SimulationResults input ----------------------------

  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # needs testthat 3rd edition, plus the team seems to be doubtful about
  # snapshot testing. So this is just for my own testing
  # expect_snapshot(print(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1255L, 8L))

  expect_equal(
    names(df2),
    c(
      "dataType", "IndividualId", "xValues", "paths", "yValues",
      "yUnit", "yDimension", "xUnit"
    )
  )

  expect_equal(
    as.character(na.omit(unique(df2$paths))),
    c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )
})


test_that("DataCombined works with population", {
  skip_if(.Platform$OS.type != "windows")
  #ospsuite::initPKSim("C:\\Program Files\\Open Systems Pharmacology\\PK-Sim 10.0")

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

  populationResults <- runSimulation(
    simulation = sim,
    population = myPopulation
  )

  myDataComb <- DataCombined$new()
  myDataComb$addSimulationResults(populationResults, individualIds = c(1, 8, 10, 44))
  df <- myDataComb$toDataFrame()

  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(1964L, 8L))

  expect_equal(
    names(df),
    c(
      "dataType", "IndividualId", "xValues", "paths", "yValues",
      "yUnit", "yDimension", "xUnit"
    )
  )

  expect_equal(unique(df$IndividualId), c(1, 8, 10, 44))
  expect_equal(
    unique(df$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(unique(df$yUnit), "µmol/l")
  expect_equal(unique(df$yDimension), "Concentration (molar)")
  expect_equal(unique(df$xUnit), "min")
})
