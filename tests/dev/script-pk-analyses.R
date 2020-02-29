library(ospsuite)

sim <- loadSimulation("tests/data/S1.pkml")

population <- loadPopulation("tests/data/pop_10.csv")

simRunOptions <- SimulationRunOptions$new(numberOfCoresToUse = 4, checkForNegativeValues = TRUE, showProgress = TRUE)


populationResults <- runSimulation(sim, population, simRunOptions)


dynamicPkParameter <- DynamicPKParameter$new(name = "test", standardPKParameter = StandardPKParameter$Cmin)
dynamicPkParameter$startApplicationIndex <- 2
dynamicPkParameter$normalizationFactor <- 2.5
dynamicPkParameter$standardPKParameter <- StandardPKParameter$Cmax
print(dynamicPkParameter)

populationPkAnalyses <- calculatePKAnalyses(populationResults, dynamicPKParameters = dynamicPkParameter)

exportPKAnalysesToCSV(populationPkAnalyses, "C:/temp/export/pk.csv")


newPKAnalyses <- importPKAnalysesFromCSV("C:/temp/export/pk.csv", sim)

df <- pkAnalysesAsDataFrame(newPKAnalyses)


tree <- exploreSimulation(sim)
path <- tree$Organism$Kidney$Interstitial$pH$path


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
