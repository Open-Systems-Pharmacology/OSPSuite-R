library(ospsuite)
library(profvis)
sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")

individualResults <- runSimulation(sim)
resultsPaths <- individualResults$allQuantityPaths

population <- loadPopulation("C:/projects/OSPSuite-R/tests/data/pop_10.csv")
populationResults <- runSimulation(sim, population)
resultsPaths <- individualResults$allQuantityPaths
#

results <- getOutputValues(individualResults, resultsPaths, individualIds = 1)

path <- resultsPaths[[1]]
results <- getOutputValuesTLF(populationResults, population, path, individualIds = c(0, 1))
