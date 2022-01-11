test_that("dataCombined - initialization", {
  skip_if_not_installed("R6")

  # initialize empty object
  myCombDat <- DataCombined$new()

  expect_null(myCombDat$dataSets)
  expect_null(myCombDat$simulationResults)
  expect_equal(length(purrr::compact(myCombDat$groups)), 0L)
})

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

  # create object with datasets combined
  myCombDat <- DataCombined$new()
  myCombDat$addSimulationResults(
    simResults,
    quantitiesOrPaths = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )
  myCombDat$addDataSets(dataSet)

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

  expect_equal(
    as.character(unique(df$name)),
    c(
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_total",
      "Stevens_2012_placebo.Placebo_proximal",
      "Stevens_2012_placebo.Sita_proximal",
      "Stevens_2012_placebo.Placebo_distal",
      "Stevens_2012_placebo.Sita_dist",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )

  # read-only
  expect_equal(dataSet, myCombDat$dataSets)
  expect_error(myCombDat$dataSets(dataSet))
  expect_equal(simResults, myCombDat$simulationResults)
  expect_error(myCombDat$simulationResults(simResults))

  # not specified, so NULL
  expect_null(myCombDat$groupMap)

  # with DataSet input ----------------------------

  # create object with datasets combined
  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)
  myCombDat2$addDataSets(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1267L, 21L))

  # not specified, so NULL
  expect_null(myCombDat$groupMap)

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

  expect_equal(
    as.character(unique(df2$name)),
    c(
      "Stevens_2012_placebo.Placebo_total",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention"
    )
  )

  # with nonsense list inputs ----------------------------

  myCombDat3 <- DataCombined$new()
  expect_error(myCombDat3$addDataSets(list("x" = 1, "y" = 2)))

  expect_error(myCombDat3$addSimulationResults(list(simResults, simResults)))

  # order should not matter ----------------------------

  # you can use one add method, extract a dataframe, and then add another add method
  # and then extract again; the dataframe method should then combine
  myCombDat4 <- DataCombined$new()
  myCombDat4$addSimulationResults(simResults)
  df3 <- myCombDat4$toDataFrame()
  myCombDat4$addDataSets(dataSet[[1]])
  df4 <- myCombDat4$toDataFrame()
  expect_equal(dim(df3), c(1255L, 9L))
  expect_equal(dim(df4), c(1267L, 21L))

  # not specified, so NULL
  expect_null(myCombDat$groupMap)
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
  myCombDat$addDataSets(dataSet[[1]])

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
  expect_equal(dim(df2), c(1255L, 9L))

  expect_equal(
    names(df2),
    c(
      "dataType", "IndividualId", "xValues", "paths", "yValues",
      "yUnit", "yDimension", "xUnit", "name"
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

# data transformations ---------------------------------

test_that("DataCombined with data transformations", {
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

  # create object with datasets combined
  myCombDat <- DataCombined$new()

  # this should fail because of incorrect argument type
  expect_error(myCombDat$addSimulationResults(simResults, groups = 2))
  expect_error(myCombDat$addDataSets(dataSet, groups = c(2, 4)))

  myCombDat$addSimulationResults(simResults)
  myCombDat$addDataSets(dataSet)

  names_ls <- list(
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
    "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
    "Stevens_2012_placebo.Sita_proximal",
    "Stevens_2012_placebo.Placebo_distal"
  )

  # original dataframe
  dfOriginal <- myCombDat$toDataFrame()
  dfOriginal <- dplyr::filter(dfOriginal, name %in% names_ls)

  # expect error since the lengths of argument are not the same
  # TODO: make sure that this errors
  # expect_error(
  #   myCombDat$setDataTransforms(
  #     names = names_ls,
  #     xOffsets = list(2, 4, 5, 5, 6)
  #   )
  # )

  myCombDat$setDataTransforms(
    names = names_ls,
    xOffsets = 2,
    yOffsets = 4,
    xScaleFactors = 1.5,
    yScaleFactors = 2.5
  )

  dfTransformed <- myCombDat$toDataFrame()

  expect_equal(dfTransformed$xValues, (dfOriginal$xValues + 2) * 1.5)
  expect_equal(dfTransformed$yValues, (dfOriginal$yValues + 4) * 2.5)
  expect_equal(dfTransformed$yErrorValues, dfOriginal$yErrorValues * 2.5)

  # create object with datasets combined
  myCombDat2 <- DataCombined$new()
  myCombDat2$addDataSets(dataSet)
  myCombDat2$addSimulationResults(simResults)

  # should error with inappropriate arguments
  expect_error(myCombDat2$setDataTransforms(
    names = 2,
    xOffsets = "2"
  ))

  expect_error(myCombDat2$setDataTransforms(
    names = c(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Stevens_2012_placebo.Sita_proximal"
    ),
    xOffsets = c("2", 3),
    xScaleFactors = c("1.5", 2.4)
  ))

  myCombDat2$setDataTransforms(
    names = list(
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Stevens_2012_placebo.Sita_proximal"
    ),
    # mix atomic vectors and lists to make sure that both work
    xOffsets = list(2, 3),
    yOffsets = c(4, 7),
    xScaleFactors = list(1.5, 2.4),
    yScaleFactors = c(1.1, 2.2)
  )

  df <- myCombDat2$toDataFrame()
  expect_equal(
    head(df$xValues),
    c(
      9.52223539352417, 46.3355529785156, 80.7838302612305, 115.264205932617,
      154.742211914062, 186.81474609375
    )
  )

  expect_equal(
    head(df$yValues),
    c(
      179.262065076828, 148.917247438431, 126.917242193222, 107.193100237846,
      89.7448281288147, 75.3310338497162
    )
  )

  expect_equal(
    na.omit(df$yErrorValues),
    structure(c(
      10.6206896156073, 4.55172412097454, 6.82758618146181,
      3.79310343414545, 6.06896549463272, 5.31034480780363, 5.31034480780363,
      5.31034480780363, 6.06896549463272, 6.06896549463272, 4.55172412097454,
      0, 0
    ), na.action = structure(14:264, class = "omit"))
  )
})

# data grouping works ---------------------------------

test_that("DataCombined works with data grouping", {
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

  # create object with datasets combined
  myCombDat <- DataCombined$new()

  # expect error when grouping length is inaccurate
  expect_error(myCombDat$addSimulationResults(simResults, groups = list(2, 4)))
  expect_error(myCombDat$addDataSets(dataSet, groups = list(1)))

  # proper grouping
  myCombDat$addSimulationResults(
    simResults,
    groups = list(NULL, NULL, "distal", "proximal", "total")
  )
  myCombDat$addDataSets(dataSet,
    groups = list("total", "total", "proximal", "proximal", "distal", "distal")
  )

  # order should matter
  myCombDat2 <- DataCombined$new()
  myCombDat2$addDataSets(dataSet,
    groups = list("total", "total", "proximal", "proximal", "distal", "distal")
  )
  myCombDat2$addSimulationResults(
    simResults,
    groups = list(NULL, NULL, "distal", "proximal", "total")
  )

  # check mapping
  dfMap <- myCombDat$groupMap
  dfMap2 <- myCombDat2$groupMap

  expect_equal(
    dfMap$group,
    c(
      "distal",
      "distal",
      "distal",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "proximal",
      "proximal",
      "proximal",
      "total",
      "total",
      "total"
    )
  )

  expect_equal(
    dfMap$name,
    c(
      "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
      "Stevens_2012_placebo.Placebo_distal",
      "Stevens_2012_placebo.Sita_dist",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric emptying",
      "Organism|Lumen|Stomach|Dapagliflozin|Gastric retention",
      "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
      "Stevens_2012_placebo.Placebo_proximal",
      "Stevens_2012_placebo.Sita_proximal",
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_total"
    )
  )

  expect_equal(dfMap$group, dfMap2$group)
  expect_equal(dfMap$name, dfMap2$name)

  # check dataframe
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(1332L, 22L))
  expect_equal(
    names(df),
    c(
      "group", "name", "dataType", "xValues", "yValues", "yErrorValues",
      "xDimension", "xUnit", "yDimension", "yUnit", "yErrorType", "yErrorUnit",
      "molWeight", "lloq", "Source", "Sheet", "Organ", "Compartment",
      "Molecule", "Group Id", "IndividualId", "paths"
    )
  )

  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")
  expect_equal(dim(df2), c(1332L, 22L))
  expect_equal(
    names(df2),
    c(
      "group", "name", "dataType", "xValues", "yValues", "yErrorValues",
      "xDimension", "xUnit", "yDimension", "yUnit", "yErrorType", "yErrorUnit",
      "molWeight", "lloq", "Source", "Sheet", "Organ", "Compartment",
      "Molecule", "Group Id", "IndividualId", "paths"
    )
  )
})

# population objects work ---------------------------------

test_that("DataCombined works with population", {
  skip_if(.Platform$OS.type != "windows")
  # ospsuite::initPKSim("C:\\Program Files\\Open Systems Pharmacology\\PK-Sim 10.0")

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
  expect_equal(dim(df), c(1964L, 9L))

  expect_equal(
    names(df),
    c(
      "dataType", "IndividualId", "xValues", "paths", "yValues",
      "yUnit", "yDimension", "xUnit", "name"
    )
  )

  expect_equal(min(df$IndividualId), 1)
  expect_equal(max(df$IndividualId), 44)
  expect_equal(
    unique(df$paths),
    "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
  )
  expect_equal(unique(df$yUnit), "µmol/l")
  expect_equal(unique(df$yDimension), "Concentration (molar)")
  expect_equal(unique(df$xUnit), "min")
})
