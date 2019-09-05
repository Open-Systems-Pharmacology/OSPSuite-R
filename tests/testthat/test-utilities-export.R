context("exportResultsToCSV")

sim <- loadTestSimulation("S1")
results <- runSimulation(sim)
pkAnalyses <- calculatePKAnalyses(results, sim);

test_that("It can export valid simulation results to CSV", {

  executeWithTestFile(function(csvFile){
    results <- runSimulation(sim)
    exportResultsToCSV(results, sim, csvFile)
    expect_true(file.exists(csvFile))
  })
})

test_that("It can export valid pk-analyses results to CSV", {

  executeWithTestFile(function(csvFile){
    results <- runSimulation(sim)
    exportResultsToCSV(results, sim, csvFile)
    expect_true(file.exists(csvFile))
  })
})

