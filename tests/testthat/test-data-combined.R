test_that("it can set and retrieve the dimension of a data column", {
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
  # TODO: turn on only when implementation is finalized
  # expect_snapshot(myCombDat)

  # checking dataframe methods
  df <- myCombDat$toDataFrame()
  expect_s3_class(df, "data.frame")

  # with DataSet input ----------------------------

  # created object with datasets combined
  myCombDat2 <- DataCombined$new()
  myCombDat2$addSimulationResults(simResults)
  myCombDat2$addDataSet(dataSet[[1]])

  expect_true(R6::is.R6(myCombDat2))
  expect_false(R6::is.R6Class(myCombDat2))
  # TODO: turn on only when implementation is finalized
  # expect_snapshot(myCombDat2)

  # checking dataframe methods
  df2 <- myCombDat2$toDataFrame()
  expect_s3_class(df2, "data.frame")

  # with nonsense DataSet list input ----------------------------

  myCombDat3 <- DataCombined$new()
  expect_error(myCombDat3$addDataSet(list("x" = 1, "y" = 2)))
})
