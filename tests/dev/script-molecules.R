library(ospsuite)

# sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/concentration_based.pkml")
sim <- loadSimulation("C:/tests/9.0/R Generation/Vergin 1995 IV.pkml")

allDrugs <- sim$allXenobioticMoleculeNames()
# molecules <- getAllMoleculesMatching("Organism|**", sim)
# for (molecule in molecules) {
#   print(molecule)
# }
#
