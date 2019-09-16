library(ospsuite)

sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")
parameter <- getParameter("Organism|Liver|Volume", sim)


addOutputs("Organism|*|Plasma|Caffeine", simulation = sim)
addOutputs(parameter, simulation = sim)

# print(outputSelections)

# parameter <- getParameter("Organism|Liver|Volume", sim)
# print(parameter)


population <- loadPopulation("C:/projects/OSPSuite-R/tests/data/pop_10.csv")
print(population)


results <- load
# results <- runSimulation(sim)
exportResultsToCSV(results, sim, "C:/temp/export/results.csv")
#
# pkAnalyses <- calculatePKAnalyses(results, sim)
#
# exportPKAnalysesToCSV(pkAnalyses, sim, "C:/temp/export/pk.csv")
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


saveSimulation(sim, "c:/temp/export/toto.xml")
