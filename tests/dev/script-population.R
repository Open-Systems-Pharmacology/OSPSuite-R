library(ospsuite)

popFile <- getTestDataFilePath("pop.csv")
population <- loadPopulation(popFile)
sim <- loadSimulation("tests/data/S1.pkml")

saveSimulation(sim, "tests/data/S2.pkml")
values <- c(1:10) * 2.5
population$setParameterValues("Organism|Lung|Volume", values)

population$getParameterValuesForIndividual(individualId = 0)
ids <- population$allIndividualIds

df <- populationToDataFrame(population = population)


res <- runSimulations(simulations = sim, population = population)[[1]]


values[2] <- NA
values[4] <- NaN
population$setParameterValues("Organism|Lung|Volume", values)
res <- runSimulations(simulations = sim, population = population)[[1]]

agingData_baby <- loadAgingDataFromCSV("tests/data/baby_aging.csv")

aging_data <- loadAgingDataFromCSV("inst/extdata/aging_data.csv")

res <- runSimulations(
  simulations = sim,
  population = population,
  agingData = aging_data
)[[1]]
