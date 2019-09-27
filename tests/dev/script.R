library(ospsuite)

sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")
parameter <- getParameter("Organism|Liver|Volume", sim)

solver <- sim$settings$solver
print(solver)

addOutputs("Organism|*|Plasma|Caffeine", simulation = sim)
addOutputs(parameter, simulation = sim)


schema <- sim$outputSchema
# print(schema)

# schema$addTimePoints(c(10, 20))
# print(schema)

# print(outputSelections)

# parameter <- getParameter("Organism|Liver|Volume", sim)
# print(parameter)


population <- loadPopulation("C:/projects/OSPSuite-R/tests/data/pop_10.csv")
# print(population)

simRunOptions <- SimulationRunOptions$new(numberOfCoresToUse = 4, checkForNegativeValues = TRUE)

individualResults <- runSimulation(sim)
paths <- individualResults$allQuantityPaths
individualPkkAnalyses <- calculatePKAnalyses(individualResults)


populationResults <- runSimulation(sim, population, simRunOptions)
populationPkAnalyses <- calculatePKAnalyses(populationResults)

exportResultsToCSV(populationResults, "C:/temp/export/results.csv")
#
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
