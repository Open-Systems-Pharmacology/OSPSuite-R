context("exportResultsToCSV")

sim <- loadTestSimulation("S1")
results <- runSimulation(sim)

test_that("It can export valid simulation results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportResultsToCSV(results, sim, csvFile)
    expect_true(file.exists(csvFile))
  })
})

context("importResultsFromCSV")

test_that("It can import valid simulation results from one CSV file", {
  resFile <- getTestDataFilePath("res_10.csv")
  results <- importResultsFromCSV(sim, resFile)
  expect_equal(results$count, 10)
})

test_that("It can import valid simulation results from multiple CSV files", {
  res1_10File <- getTestDataFilePath("res_10.csv")
  res11_20File <- getTestDataFilePath("res_11-20.csv")
  results <- importResultsFromCSV(sim, c(res1_10File, res11_20File))
  expect_equal(results$count, 20)
})

test_that("It throws an exception if the file imported are not valid results file", {
  junkFile <- getTestDataFilePath("pop_10.csv")
  expect_that(importResultsFromCSV(sim, junkFile), throws_error())
})

test_that("It throws an exception when importing a valid result file that does not match the simulation", {
  otherSim <- loadTestSimulation("simple")
  resFile <- getTestDataFilePath("res_10.csv")
  expect_that(importResultsFromCSV(otherSim, resFile), throws_error())
})
