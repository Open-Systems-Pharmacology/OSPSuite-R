library(ospsuite)

sim <- loadSimulation("tests/data/S1.pkml")
popFile <- getTestDataFilePath("pop.csv")
pop <- loadPopulation(popFile)

demographyParameters <- getAllParametersMatching(StandardPath$BMI, sim)

demographyValues <- lapply(demographyParameters, function(p) {
  toDisplayUnit(p, pop$getParameterValues(p))
})

cyp3A4Parameters <- getStandardMoleculeParameters("CYP3A4", sim)
for (p in cyp3A4Parameters) {
  print(p)
}
