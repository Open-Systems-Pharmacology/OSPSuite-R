# Creates and returns an instance of a `SimulationBatch` that can be used to efficiently vary parameters and initial values in a simulation

Creates and returns an instance of a `SimulationBatch` that can be used
to efficiently vary parameters and initial values in a simulation

## Usage

``` r
createSimulationBatch(
  simulation,
  parametersOrPaths = NULL,
  moleculesOrPaths = NULL
)
```

## Arguments

- simulation:

  Instance of a `Simulation` to simulate in a batch mode

- parametersOrPaths:

  Parameter instances (element or vector) typically retrieved using
  `getAllParametersMatching` or parameter path (element or vector of
  strings) that will be varied in the simulation. (optional) When
  providing the paths, only absolute full paths are supported (i.e., no
  matching with '\*' possible). If parametersOrPaths is `NULL`, you will
  not be able to set parameter values during batch run.

- moleculesOrPaths:

  Molecule instances (element or vector) typically retrieved using
  `getAllMoleculesMatching` or molecule path (element or vector of
  strings) that will be varied in the simulation. (optional) When
  providing the paths, only absolute full paths are supported (i.e., no
  matching with '\*' possible). If moleculesOrPaths is `NULL`, you will
  not be able to set molecule initial values during batch run.

## Value

SimulationBatch that can be used to vary parameter values or molecule
initial values and run simulation in an optimized manner

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Create a simulation batch that will allow batch run for one parameter value
simulationBatch <- createSimulationBatch(sim, "Organism|Liver|Volume")

# Create a simulation batch that will allow batch run for multiple parameter
# values and initial values
simulationBatch <- createSimulationBatch(
  sim,
  c("Organism|Liver|Volume", "R1|k1"),
  c("Organism|Liver|A")
)
```
