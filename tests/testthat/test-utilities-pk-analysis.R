# calculatePKAnalyses

sim <- loadTestSimulation("S1")
clearOutputs(sim)
outputs <- "Organism|VenousBlood|*|*"
addOutputs(outputs, sim)
results <- runSimulations(sim)[[1]]
pkAnalyses <- calculatePKAnalyses(results)

test_that("It should be able to calculate the PK-Analyses each output of a simulation", {
  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor(
    "Organism|VenousBlood|Plasma|Caffeine"
  )
  expect_gt(length(pkAnalysesForOutput), 0)
})

test_that("It should be able to retrieve a standard pk parameter", {
  pkParameter <- pkAnalyses$pKParameterFor(
    "Organism|VenousBlood|Plasma|Caffeine",
    "AUC_tEnd"
  )
  expect_false(is.null(pkParameter))
})

test_that("It should return null when retrieving a pk parameter that does not exist", {
  pkParameter <- pkAnalyses$pKParameterFor(
    "Organism|VenousBlood|Plasma|Caffeine",
    "NOPE"
  )
  expect_null(pkParameter)
})

test_that("it can return pk analysis for sparse population and only return ids defined in the population file", {
  clearOutputs(sim)
  addOutputs(outputs, sim)
  population <- loadPopulation(getTestDataFilePath("pop_5_spared_id.csv"))
  results <- runSimulations(sim, population = population)[[1]]
  pkAnalyses <- calculatePKAnalyses(results = results)
  df <- pkAnalysesToDataFrame(pkAnalyses)
  d <- dplyr::distinct(df, IndividualId)
  expect_equal(nrow(d), 5)
})


test_that("It should an empty list of parameters for an output that is not part of the calculated results", {
  clearOutputs(sim)
  addOutputs(outputs, sim)
  results <- runSimulations(sim)[[1]]
  pkAnalyses <- calculatePKAnalyses(results)

  pkAnalysesForOutput <- pkAnalyses$allPKParametersFor(
    "Another output that does not exist"
  )
  expect_equal(length(pkAnalysesForOutput), 0)
})

# exportPKAnalysesToCSV

test_that("It can export valid pk-analyses results to CSV", {
  executeWithTestFile(function(csvFile) {
    exportPKAnalysesToCSV(pkAnalyses, csvFile)
    expect_true(file.exists(csvFile))
  })
})

# pkAnalysesToDataFrame

test_that("It can convert valid pk-analysis results to data frame", {
  df <- pkAnalysesToDataFrame(pkAnalyses = pkAnalyses)
  expect_length(colnames(df), 5)
})

test_that("calculatePKAnalyses works with a list of SimulationResults", {
  sim1 <- loadTestSimulation("S1")
  sim2 <- loadTestSimulation("S1")
  
  clearOutputs(sim1)
  clearOutputs(sim2)
  addOutputs(outputs, sim1)
  addOutputs(outputs, sim2)
  
  simulationResults <- runSimulations(simulations = list(sim1, sim2))
  pkAnalysesList <- calculatePKAnalyses(simulationResults)
  
  # Should return a list of SimulationPKAnalyses
  expect_true(is.list(pkAnalysesList))
  expect_equal(length(pkAnalysesList), 2)
  expect_true(all(sapply(pkAnalysesList, function(x) inherits(x, "SimulationPKAnalyses"))))
  
  # Each should have valid PK parameters
  pkAnalysesForOutput1 <- pkAnalysesList[[1]]$allPKParametersFor(
    "Organism|VenousBlood|Plasma|Caffeine"
  )
  expect_gt(length(pkAnalysesForOutput1), 0)
})

test_that("calculatePKAnalyses works with single result (backward compatibility)", {
  results <- runSimulations(sim)[[1]]
  pkAnalyses <- calculatePKAnalyses(results)
  
  # Should return a single SimulationPKAnalyses object, not a list
  expect_s3_class(pkAnalyses, "SimulationPKAnalyses")
  expect_true(inherits(pkAnalyses, "SimulationPKAnalyses"))
})
