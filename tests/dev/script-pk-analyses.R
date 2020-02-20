library(ospsuite)

sim <- loadSimulation("tests/data/S1.pkml")

population <- loadPopulation("tests/data/pop_10.csv")

simRunOptions <- SimulationRunOptions$new(numberOfCoresToUse = 4, checkForNegativeValues = TRUE, showProgress = TRUE)
populationResults <- runSimulation(sim, population, simRunOptions)
populationPkAnalyses <- calculatePKAnalyses(populationResults)

exportPKAnalysesToCSV(populationPkAnalyses, "C:/temp/export/pk.csv")


newPKAnalyses = importPKAnalysesFromCSV("C:/temp/export/pk.csv", sim)

df= pkAnalysesAsDataFrame(newPKAnalyses)
#
# pkParameters <- pkAnalyses$allPKParametersFor("Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)")
#
# for (pkParameter in pkParameters) {
#   print(pkParameter)
# }
#
#
# pkParam <- pkParameters[[2]]
#
# val <- pkParam$values
# getPkAnalysis(results)


# individualResults <- results[[1]]
#
# time <- rClr::clrGet(individualResults, "Time")
#
# allValues <- rClr::clrCall(individualResults, "ValuesAsArray")
#
# firstOutput <- allValues[[1]]
# path <- rClr::clrGet(firstOutput, "QuantityPath")
# values <- rClr::clrGet(firstOutput, "Values")


# saveSimulation(sim, "c:/temp/export/toto.xml")
