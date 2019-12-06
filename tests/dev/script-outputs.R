library(ospsuite)
sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")

individualResults <- runSimulation(sim)
resultsPaths <- individualResults$allQuantityPaths

tlf <- getOutputValuesTLF(individualResults)
print(tlf)

population <- loadPopulation("C:/projects/OSPSuite-R/tests/data/pop_10.csv")
populationResults <- runSimulation(sim, population)
resultsPaths <- populationResults$allQuantityPaths

results <- getOutputValues(populationResults, resultsPaths, individualIds = 1)

path <- resultsPaths[[1]]
results <- getOutputValuesTLF(populationResults, path, population = population, individualIds = c(0, 1))
