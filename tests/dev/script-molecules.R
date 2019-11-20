library(ospsuite)

#sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/concentration_based.pkml")
sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/amount_based.pkml", loadFromCache = FALSE)

molecules <- getAllMoleculesMatching("Organism|**", sim)
for (molecule in molecules) {
  print(molecule)
}

setMoleculeStartValues(molecules, 30)

for (molecule in molecules) {
  print(molecule)
}
