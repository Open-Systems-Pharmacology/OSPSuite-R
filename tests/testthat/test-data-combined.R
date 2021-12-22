test_that("dataCombined - both dataSet and SimulationResults provided", {
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

  # with list input ----------------------------

  # created object with datasets combined
  myCombDat <- DataCombined$new()
  myCombDat$addSimulationResults(simResults)
  myCombDat$addDataSet(dataSet)

  expect_true(R6::is.R6(myCombDat))
  expect_false(R6::is.R6Class(myCombDat))

  # checking dataframe methods
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(dim(df), c(1332L, 21L))

  expect_equal(
    names(df),
    c(
      "dataType", "name", "xValues", "yValues", "yErrorValues", "xDimension",
      "xUnit", "yDimension", "yUnit", "yErrorType", "yErrorUnit", "molWeight",
      "lloq", "Source", "Sheet", "Organ", "Compartment", "Molecule",
      "Group Id", "IndividualId", "paths"
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
})
