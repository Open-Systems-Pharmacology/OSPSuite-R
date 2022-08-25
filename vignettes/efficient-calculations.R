## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)

## ----SequentialRun------------------------------------------------------------
library(ospsuite)

# Load and run the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
doseParameter <- getAllParametersMatching(toPathString("Applications", "**", "Dose"), sim)[[1]]

# run for dose 100mg
doseParameter$value <- toBaseUnit(doseParameter, 100, "mg")
result100 <- runSimulations(simulations = sim)

# run for dose 200mg
doseParameter$value <- toBaseUnit(doseParameter, 200, "mg")
result200 <- runSimulations(simulations = sim)

# ...

# run for dose 500mg
doseParameter$value <- toBaseUnit(doseParameter, 500, "mg")
result500 <- runSimulations(simulations = sim)

## ----ParallelRun, eval=TRUE---------------------------------------------------
library(ospsuite)
# Load and run the simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

loadSimulationWithDose <- function(doseInMg) {
  sim <- loadSimulation(simFilePath, loadFromCache = FALSE)
  doseParameter <- getAllParametersMatching(toPathString("Applications", "**", "Dose"), sim)[[1]]
  doseParameter$value <- toBaseUnit(doseParameter, doseInMg, "mg")
  return(sim)
}

# Creates 5 instances of a simulation (This is very fast for typical simulations)
sim100 <- loadSimulationWithDose(doseInMg = 100)
sim200 <- loadSimulationWithDose(doseInMg = 200)
sim300 <- loadSimulationWithDose(doseInMg = 300)
sim400 <- loadSimulationWithDose(doseInMg = 400)
sim500 <- loadSimulationWithDose(doseInMg = 500)


# Runs the simulation in parallel
results <- runSimulations(simulations = list(sim100, sim200, sim300, sim400, sim500))

# Results in now a list of SimulationResults

## ----BatchRunInit, eval=TRUE--------------------------------------------------
library(ospsuite)
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")

# We load the simulation for which the batches will be created
sim1 <- loadSimulation(simFilePath, loadFromCache = FALSE)

# define the list of parameter that will be varied between the runs.
# For the first batch, we will vary 2 parameters: Lipophilicity and Permeability
parameterPaths <- c("Aciclovir|Lipophilicity", "Aciclovir|Permeability")

# define a first simulation batch
simBatch1 <- createSimulationBatch(simulation = sim1, parametersOrPaths = parameterPaths)

# for the second batch, we will vary Molecular Weight
simBatch2 <- createSimulationBatch(
  simulation = sim1,
  parametersOrPaths = c("Aciclovir|Molecular weight")
)

simulationBatches <- list(simBatch1, simBatch2)

## ----BatchRunEnque, eval=TRUE-------------------------------------------------
# now setting some parameter run values (the size of the array should match
# the number of parameters to vary for each batch
simBatch1$addRunValues(parameterValues = c(1, 2))
simBatch1$addRunValues(parameterValues = c(3, 4))
simBatch1$addRunValues(parameterValues = c(5, 6))

# We only have one parameter to vary for simBatch2, therefore only one value to set
simBatch2$addRunValues(parameterValues = 150)
simBatch2$addRunValues(parameterValues = 200)
simBatch2$addRunValues(parameterValues = 300)
simBatch2$addRunValues(parameterValues = 400)

## ----BatchRunRun, eval=TRUE---------------------------------------------------
# Now we run the simulation batches.
# By doing so, 7 runs (3 for simBatch1 and 4 for simBatch2) will be executed in parallel.
# Please see documentation of runSimulationBatches for more details.
# The resulting output is a named list, where the names are the ids of the enqueued runs.
results <- runSimulationBatches(simulationBatches)
print(names(unlist(results)))

## ----BatchRunRunNewValues, eval=TRUE------------------------------------------
simBatch1$addRunValues(parameterValues = c(10, 20))
simBatch1$addRunValues(parameterValues = c(30, 40))
simBatch2$addRunValues(parameterValues = 500)
simBatch2$addRunValues(parameterValues = 200)

# this run will be much faster as the simulation won't be initialized again.
# Only the new value will be set as specified when adding new run values with addRunValues
results2 <- runSimulationBatches(simulationBatches)

# Once the simulation batches instances are not needed anymore, they can be removed
# from the environment and the allocated memory cleared.
# This will be done automatically when the R session is terminated.
rm(simBatch1)
rm(simBatch2)

