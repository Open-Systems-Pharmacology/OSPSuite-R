library(ospsuite)

population <- loadPopulation("tests/data/pop_10.csv")

population$getParameterValuesForIndividual(individualId = 10)
ids <- population$allIndividualIds

df<- populationAsDataFrame(population = population)
