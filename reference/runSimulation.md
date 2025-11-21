# Run a single simulation

**\[deprecated\]**

## Usage

``` r
runSimulation(
  simulation,
  population = NULL,
  agingData = NULL,
  simulationRunOptions = NULL
)
```

## Arguments

- simulation:

  One `Simulation` to simulate.

- population:

  Optional instance of a `Population` to use for the simulation. This is
  only used when simulating one simulation Alternatively, you can also
  pass the result of `createPopulation` directly. In this case, the
  population will be extracted

- agingData:

  Optional instance of `AgingData` to use for the simulation. This is
  only used with a population simulation

- simulationRunOptions:

  Optional instance of a `SimulationRunOptions` used during the
  simulation run

## Value

SimulationResults (one entry per Individual) for a single simulation

## Details

Runs one simulation (individual or population) and returns a
`SimulationResults` object containing all results of the simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Running an individual simulation
# results is an instance of `SimulationResults`
results <- runSimulation(sim)

# Creating custom simulation run options

simRunOptions <- SimulationRunOptions$new()
simRunOptions$numberOfCores <- 3
simRunOptions$showProgress <- TRUE

# Running a population simulation
popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
population <- loadPopulation(popPath)
results <- runSimulation(sim, population, simulationRunOptions = simRunOptions)
} # }
```
