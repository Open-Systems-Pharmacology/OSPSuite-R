
library(ospsuite)

sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")
pop <- loadPopulation("C:/projects/OSPSuite-R/tests/data/pop_10.csv")

demographyParameters <- getAllParametersMatching(c("Organism|BMI", "Organism|Age", "Organism|Weight", "Organism|Height"), sim)

demographyValues <- lapply(demographyParameters, function(p) toDisplayUnit(p, pop$getValues(p)))
