#Create Individual example.
#Assuming PKSim installed

initPKSim("C:/projects/PK-Sim/src/PKSim.R/bin/Debug/net472")


originData <- OriginData$new()
originData$species <- Species$Human
originData$population <- HumanPopulation$European_ICRP_2002
originData$gender <- Gender$Female

print(originData)

originData <- createOriginData(
  species =  Species$Human,
  population =  HumanPopulation$European_ICRP_2002,
  gender = Gender$Female,
  age = 10,
  height = 175,
  weight = 60
)

print(originData)

moleculeOntogeny <- MoleculeOntogeny$new()
moleculeOntogeny$molecule <- "CYP3A4"
moleculeOntogeny$ontogeny <- "CYP3A4"
parameterValues <- createIndividual(originData = originData, moleculeOntogenies =  c(moleculeOntogeny))
distributedValues <- createIndividual(originData = originData, useDistribution = TRUE, moleculeOntogenies =  c(moleculeOntogeny))

