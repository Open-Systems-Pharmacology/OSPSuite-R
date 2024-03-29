# Create Individual example.
# Assuming PKSim installed

initPKSim("C:/projects/PK-Sim/src/PKSim/bin/Debug/net472")

#
# individualCharacteristics <- IndividualCharacteristics$new()
# individualCharacteristics$species <- Species$Human
# individualCharacteristics$population <- HumanPopulation$European_ICRP_2002
# individualCharacteristics$gender <- Gender$Female
#
# print(individualCharacteristics)
#
moleculeOntogeny <- MoleculeOntogeny$new(molecule = "MyMolecule", ontogeny = StandardOntogeny$CYP3A4)

dog <- createIndividualCharacteristics(
  species = Species$Dog,
  weight = 10
)

print(dog)
dogValues <- createIndividual(individualCharacteristics = dog)

human <- createIndividualCharacteristics(
  species = Species$Human,
  population = HumanPopulation$Asian_Tanaka_1996,
  gender = Gender$Female,
  age = 10,
  height = 175,
  weight = 60,
  moleculeOntogenies = moleculeOntogeny
)

print(human)

parameterValues <- createIndividual(individualCharacteristics = human)
distributedValues <- createDistributions(individualCharacteristics = human)
