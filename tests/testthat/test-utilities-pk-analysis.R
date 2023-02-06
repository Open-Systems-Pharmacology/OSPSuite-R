context("calculatePKAnalyses")

sim <- loadTestSimulation("S1")
clearOutputs(sim)
outputs <- "Organism|VenousBlood|*|*"
addOutputs(outputs, sim)
results <- runSimulation(sim)
pkAnalyses <- calculatePKAnalyses(results)

test_that("It should be able to calculate the PK-Analyses each output of a simulation", {
  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor("Organism|VenousBlood|Plasma|Caffeine")
  expect_gt(length(pkAnalysesForOutput), 0)
})

test_that("It should be able to retrieve a standard pk parameter", {
  pkParameter <- pkAnalyses$pKParameterFor("Organism|VenousBlood|Plasma|Caffeine", "AUC_tEnd")
  expect_false(is.null(pkParameter))
})

test_that("It should return null when retrieving a pk parameter that does not exist", {
  pkParameter <- pkAnalyses$pKParameterFor("Organism|VenousBlood|Plasma|Caffeine", "NOPE")
  expect_null(pkParameter)
})

test_that("it can return pk analysis for sparse population and only return ids defined in the population file", {
  clearOutputs(sim)
  addOutputs(outputs, sim)
  population <- loadPopulation(getTestDataFilePath("pop_5_spared_id.csv"))
  results <- runSimulation(sim, population = population)
  pkAnalyses <- calculatePKAnalyses(results = results)
  df <- pkAnalysesToDataFrame(pkAnalyses)
  d <- dplyr::distinct(df, IndividualId)
  expect_equal(nrow(d), 5)
})


test_that("It should an empty list of parameters for an output that is not part of the calculated results", {
  clearOutputs(sim)
  addOutputs(outputs, sim)
  results <- runSimulation(sim)
  pkAnalyses <- calculatePKAnalyses(results)

  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor("Another output that does not exist")
  expect_equal(length(pkAnalysesForOutput), 0)
})

context("exportPKAnalysesToCSV")

test_that("It can export valid pk-analyses results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportPKAnalysesToCSV(pkAnalyses, csvFile)
    expect_true(file.exists(csvFile))
  })
})

context("pkAnalysesToDataFrame")

test_that("It can convert valid pk-analysis results to data frame", {
  df <- pkAnalysesToDataFrame(pkAnalyses = pkAnalyses)
  expect_length(colnames(df), 5)
})
