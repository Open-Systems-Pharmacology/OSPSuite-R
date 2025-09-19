sim <- loadTestSimulation("simple")
sensitivity <- SensitivityAnalysis$new(sim)
sensitivityAnalysisOptions <- SensitivityAnalysisRunOptions$new(
  showProgress = FALSE
)
results <- runSensitivityAnalysis(sensitivity, sensitivityAnalysisOptions)


# potentialVariableParameterPathsFor
test_that("It returns an array of parameter path that can potentially be varied in the simulation", {
  parameterPath <- potentialVariableParameterPathsFor(sim)
  expect_gt(length(parameterPath), 0)
})

# exportSensitivityAnalysisResultsToCSV

test_that("It can export valid simulation results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportSensitivityAnalysisResultsToCSV(results, csvFile)
    expect_true(file.exists(csvFile))
  })
})

# importSensitivityAnalysisResultsFromCSV

test_that("It can import valid simulation results from one CSV file", {
  resFile <- getTestDataFilePath("sa.csv")
  importedResults <- importSensitivityAnalysisResultsFromCSV(sim, resFile)
  expect_equal(importedResults$count, 24)
})

test_that("It save the reference to the original simulation", {
  resFile <- getTestDataFilePath("sa.csv")
  importedResults <- importSensitivityAnalysisResultsFromCSV(sim, resFile)
  expect_equal(importedResults$simulation, sim)
})

test_that("It can import valid simulation results from multiple CSV files", {
  sa1_file <- getTestDataFilePath("sa_1.csv")
  sa2_file <- getTestDataFilePath("sa_2.csv")
  importedResults <- importSensitivityAnalysisResultsFromCSV(
    sim,
    c(sa1_file, sa2_file)
  )
  expect_equal(importedResults$count, 24)
})

test_that("It throws an exception if the file imported are not valid results file", {
  junkFile <- getTestDataFilePath("pop.csv")
  expect_error(importSensitivityAnalysisResultsFromCSV(sim, junkFile))
})

test_that("It throws an exception when importing a valid result file that would result in duplicated results", {
  sa1_file <- getTestDataFilePath("sa_1.csv")
  expect_error(importSensitivityAnalysisResultsFromCSV(
    sim,
    c(sa1_file, sa1_file)
  ))
})
