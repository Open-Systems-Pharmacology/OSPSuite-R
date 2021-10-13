library(ospsuite)

population <- loadPopulation("tests/data/pop_10.csv")
sim <- loadSimulation("tests/data/S1.pkml")

saveSimulation(sim, "tests/data/S2.pkml")
values <- c(1:10) * 2.5
population$setParameterValues("Organism|Lung|Volume", values)

population$getParameterValuesForIndividual(individualId = 0)
ids <- population$allIndividualIds

df <- populationAsDataFrame(population = population)


res <- runSimulation(simulation = sim, population = population)


values[2] <- NA
values[4] <- NaN
population$setParameterValues("Organism|Lung|Volume", values)
res <- runSimulation(simulation = sim, population = population)

agingData_baby <- loadAgingDataFromCSV("tests/data/baby_aging.csv")

aging_data <- loadAgingDataFromCSV("inst/extdata/aging_data.csv")

res <- runSimulation(simulation = sim, population = population, agingData = aging_data)
