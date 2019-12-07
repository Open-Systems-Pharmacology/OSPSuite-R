
library(ospsuite)

sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")
pop <- loadPopulation("C:/projects/OSPSuite-R/tests/data/pop_10.csv")

demographyParameters <- getAllParametersMatching(c(StandardPath$Age, StandardPath$Weight, StandardPath$Height), sim)

demographyValues <- lapply(demographyParameters, function(p) toDisplayUnit(p, pop$getValues(p)))


cyp3A4Parameters <- getAllMoleculeParameters("CYP3A4", sim)
for (p in cyp3A4Parameters) {
  print(p)
}
