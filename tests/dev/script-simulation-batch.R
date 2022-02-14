library(ospsuite)

sim <- loadSimulation("tests/data/S1.pkml")

parameter <- getParameter("Organism|Liver|Volume", sim)
molecule <- getMolecule("Organism|Kidney|Intracellular|Caffeine", sim)


# Create a simulation batch instance that will hold a reference to the simulation as well as manage the parameter and molecule to vary
simulationBatch <- createSimulationBatch(sim, parametersOrPaths = c(parameter), moleculesOrPaths = molecule)

# Run the simulation by updating parameters and initial values
# Order of values is based on the order with which the parameters or initial values were defined
res <- simulationBatch$run(parameterValues = 1.2, initialValues = c(2.5))
values <- getOutputValues(res)
data <- values$data$`Organism|ArterialBlood|Plasma|Caffeine|Plasma Unbound`


# Update parameters and run again. The simulation is initialized only once
res <- simulationBatch$run(parameterValues = 4, initialValues = c(4))
