library(ospsuite)
sim <- loadSimulation("tests/data/S1.pkml")

sim$solver$absTol <- 1e-13

individualResults <- runSimulations(sim)[[1]][[1]]
resultsPaths <- individualResults$allQuantityPaths

tlf <- getOutputValues(individualResults)
print(tlf)

popFile <- getTestDataFilePath("pop.csv")
population <- loadPopulation(popFile)
populationResults <- runSimulations(sim, population)[[1]]
resultsPaths <- populationResults$allQuantityPaths

path <- resultsPaths[[1]]
results <- getOutputValues(populationResults, path, population = population, individualIds = c(0, 1))
