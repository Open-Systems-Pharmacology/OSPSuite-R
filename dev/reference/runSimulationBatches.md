# Run simulation batches

Run simulation batches

## Usage

``` r
runSimulationBatches(
  simulationBatches,
  simulationRunOptions = NULL,
  silentMode = FALSE,
  stopIfFails = FALSE
)
```

## Arguments

- simulationBatches:

  List of `SimulationBatch` objects with added parameter and initial
  values

- simulationRunOptions:

  Optional instance of a `SimulationRunOptions` used during the
  simulation run.

- silentMode:

  If `TRUE`, no warnings are displayed if a simulation fails. Default is
  `FALSE`. Has no effect if `stopIfFails` is `TRUE`.

- stopIfFails:

  Whether to stop the execution if one of the simulations failed.
  Default is `FALSE`.

## Value

Nested list of `SimulationResults` objects. The first level of the fist
are the IDs of the SimulationBatches, containing a list of
`SimulationResults` for each set of parameter/initial values. If a
simulation with a parameter/initial values set fails, the result for
this run is `NULL`

## Details

Runs a set of simulation batches. The simulation batches must be
populated with sets of parameter and start values with
`SimulationBatch$addRunValues()` prior to running. After the run, the
list of parameter and start values is cleared.

## Examples

``` r
if (FALSE) { # \dontrun{
sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
parameters <- c("Organism|Liver|Volume", "R1|k1")
molecules <- "Organism|Liver|A"
# Create two simulation batches.
simulationBatch1 <- createSimulationBatch(
  simulation = sim1,
  parametersOrPaths = parameters,
  moleculesOrPaths = molecules
)
simulationBatch2 <- createSimulationBatch(
  simulation = sim2,
  parametersOrPaths = parameters,
  moleculesOrPaths = molecules
)
# Ids of run values
ids <- c()
ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
} # }
```
