library(ospsuite)

# library(profvis)
sim <- loadSimulation("tests/data/S1.pkml")

sim <- loadSimulation("tests/data/S1.pkml")
container <- getContainer("Organism|Liver", sim)
container$containerType

distributedParameter <- getParameter("Organism|Liver|Volume", sim)
formulaParameter <- getParameter("Organism|Weight", sim)
constantParameter <- getParameter("Organism|Age", sim)

# sim2 <- loadSimulation("tests/data/simple.pkml")
# tableParameter <- getParameter("Organism|TableParameter", sim2)
# #
# print(distributedParameter)
# print(formulaParameter)
# print(constantParameter)
# print(tableParameter)
#
# tableFormula <- tableParameter$formula
# tableFormula$addPoint(50, 5)
#
# print(tableParameter)
#
# tableFormula$removePoint(30, 3)
# print(tableParameter)
#
# points <- tableFormula$allPoints
# points[[3]]$restartSolver <- TRUE
#
# print(constantParameter)
# scaleParameterValues(constantParameter, 1.5)
# print(constantParameter)

# tableParameter$value <- 5
# print(tableParameter)
#
# tableParameter$reset()
# print(tableParameter)

# print(distributedParameter)
#
# solver <- sim$$solver
# print(solver)
#
# addOutputs("Organism|*|Plasma|Caffeine", simulation = sim)
# addOutputs(parameter, simulation = sim)
#
#
# schema <- sim$outputSchema
# print(schema)

# schema$addTimePoints(c(10, 20))
# print(schema)

# print(outputSelections)

# parameter <- getParameter("Organism|Liver|Volume", sim)
# print(parameter)


population <- loadPopulation("tests/data/pop_10.csv")

exportIndividualSimulations(population = population, c(1, 2, 3), "C:/LocalNuget", sim)
# population <- loadPopulation("C:/tests/9.0/Pop_5000.csv")

# print(population)
#
simRunOptions <- SimulationRunOptions$new(numberOfCores = 4, checkForNegativeValues = TRUE, showProgress = TRUE)
#
# individualResults <- runSimulation(sim)
# paths <- individualResults$allQuantityPaths
# individualPkkAnalyses <- calculatePKAnalyses(individualResults)
#
#
# populationResults <- runSimulation(sim, population, simRunOptions)
populationResults <- importResultsFromCSV(sim, "C:/temp/export/results.csv")
# populationPkAnalyses <- calculatePKAnalyses(populationResults)
#
profvis(
  {
    outputValues <- getOutputValues(populationResults, population, populationResults$allQuantityPaths)
  },
  prof_output = "C:/temp/export/prof.html"
)

outputValues <- getOutputValues(populationResults, populationResults$allQuantityPaths, population=population, c(1))
# outputValues <- getOutputValues(populationResults, population, populationResults$allQuantityPaths)
#
# exportResultsToCSV(populationResults, "C:/temp/export/results.csv")
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


# saveSimulation(sim, "c:/temp/export/toto.xml")
