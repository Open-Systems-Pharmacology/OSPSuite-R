library(ospsuite)

sim <- loadSimulation("tests/data/S1.pkml")

parameter <- getParameter("Organism|Liver|Volume", sim)
molecule <- getMolecule("Organism|Kidney|Intracellular|Caffeine", sim)

simulationBatch <- createSimulationBatch(sim, parametersOrPaths = c(parameter), moleculesOrPaths = molecule)

res<- simulationBatch$run(parameterValues = 1.2, initialValues = c(2.5))
values <-getOutputValues(res)
data <- values$data$`Organism|ArterialBlood|Plasma|Caffeine|Plasma Unbound`


res<- simulationBatch$run(parameterValues = 4, initialValues = c(4))
