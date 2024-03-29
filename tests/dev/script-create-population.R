# Create Individual example.
# Assuming PKSim installed

initPKSim("C:/projects/PK-Sim/src/PKSim/bin/Debug/net472")

sim <- loadSimulation("tests/data/S1.pkml")

moleculeOntogeny <- MoleculeOntogeny$new(molecule = "MyMolecule", ontogeny = StandardOntogeny$CYP3A4)

dog <- createPopulationCharacteristics(
  species = Species$Dog,
  numberOfIndividuals = 50,
  weightMin = 2,
  weightMax = 5
)

print(dog)
result <- createPopulation(populationCharacteristics = dog)
populationDog <- result$population
print(populationDog)

human <- createPopulationCharacteristics(
  species = Species$Human,
  population = HumanPopulation$Asian_Tanaka_1996,
  numberOfIndividuals = 50,
  proportionOfFemales = 100,
  ageMin = 20,
  ageMax = 30,
  weightMin = 50,
  moleculeOntogenies = moleculeOntogeny
)

result <- createPopulation(populationCharacteristics = human)
populationHuman <- result$population
print(populationHuman)

populationHuman$getParameterValues("Organism|Age")


preterm <- createPopulationCharacteristics(
  species = Species$Human,
  population = HumanPopulation$Preterm,
  ageMin = 1,
  ageMax = 10,
  ageUnit = "day(s)",
  gestationalAgeMin = 26,
  gestationalAgeMax = 32,
  numberOfIndividuals = 50,
)

result <- createPopulation(populationCharacteristics = preterm)

res <- runSimulations(simulations = sim, population = result)[[1]]

populationPreterm <- result$population


populationPreterm$allParameterPaths

populationPreterm$getParameterValues("Organism|Age")
populationPreterm$getParameterValues("Organism|Gestational age")

df <- populationToDataFrame(population = populationPreterm)
