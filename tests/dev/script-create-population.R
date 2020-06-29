# Create Individual example.
# Assuming PKSim installed

initPKSim("C:/projects/PK-Sim/src/PKSim/bin/Debug/net472")

moleculeOntogeny <- MoleculeOntogeny$new(molecule = "MyMolecule", ontogeny = StandardOntogeny$CYP3A4)

dog <- createPopulationCharacteristics(
  species = Species$Dog,
  numberOfIndividuals = 50
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

print(human)

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
populationPreterm <- result$population


populationPreterm$allParameterPaths

populationPreterm$getParameterValues("Organism|Age")
populationPreterm$getParameterValues("Organism|Gestational age")

df <- populationAsDataFrame(population = populationPreterm)
