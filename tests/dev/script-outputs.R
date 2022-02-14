library(ospsuite)
sim <- loadSimulation("tests/data/S1.pkml")

sim$solver$absTol <- 1e-13

individualResults <- runSimulation(sim)
resultsPaths <- individualResults$allQuantityPaths

tlf <- getOutputValues(individualResults)
print(tlf)

popFile <- getTestDataFilePath("pop.csv")
population <- loadPopulation(popFile)
populationResults <- runSimulation(sim, population)
resultsPaths <- populationResults$allQuantityPaths

path <- resultsPaths[[1]]
results <- getOutputValues(populationResults, path, population = population, individualIds = c(0, 1))
