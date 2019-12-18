#Create Individual example.
#Assuming PKSim installed

initPKSim("C:/projects/PK-Sim/src/PKSim.R/bin/Debug/net472")

#
# individualCharacteristics <- IndividualCharacteristics$new()
# individualCharacteristics$species <- Species$Human
# individualCharacteristics$population <- HumanPopulation$European_ICRP_2002
# individualCharacteristics$gender <- Gender$Female
#
# print(individualCharacteristics)
#
moleculeOntogeny <- MoleculeOntogeny$new()
moleculeOntogeny$molecule <- "CYP3A4"
moleculeOntogeny$ontogeny <- "CYP3A4"

dog <- createIndividualCharacteristics(
  species = Species$Dog,
  weight = 10
)

dogValues <- createIndividual(individualCharacteristics = dog)

human <- createIndividualCharacteristics(
  species =  Species$Human,
  population = "toto",
  gender = Gender$Female,
  age = 10,
  height = 175,
  weight = 60,
  moleculeOntogenies =  moleculeOntogeny
  )

print(human)

parameterValues <- createIndividual(individualCharacteristics = human)
distributedValues <- createIndividual(individualCharacteristics = human, useDistribution = TRUE)

