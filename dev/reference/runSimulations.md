# Runs multiple simulations concurrently.

Runs multiple simulations concurrently.

## Usage

``` r
runSimulations(
  simulations,
  population = NULL,
  agingData = NULL,
  simulationRunOptions = NULL,
  silentMode = FALSE,
  stopIfFails = FALSE
)
```

## Arguments

- simulations:

  One `Simulation` or a list or vector of `Simulation` objects to
  simulate. List or vector can be named (names must be uniques), in
  which case the names will reused in the `simulationResults` output
  list. If not named, the output list will use simulation ids for names.

- population:

  Optional instance of a `Population` to use for the simulation. Only
  allowed when simulating one simulation. Alternatively, you can also
  pass the result of `createPopulation` directly. In this case, the
  population will be extracted.

- agingData:

  Optional instance of `AgingData` to use for the simulation. This is
  only used with a population simulation

- simulationRunOptions:

  Optional instance of a `SimulationRunOptions` used during the
  simulation run

- silentMode:

  If `TRUE`, no warnings are displayed if a simulation fails. Default is
  `FALSE`. Has no effect if `stopIfFails` is `TRUE`.

- stopIfFails:

  Whether to stop the execution if one of the simulations failed.
  Default is `FALSE`.

## Value

A named list of `SimulationResults` objects with names being the IDs of
the respective simulations. If a simulation fails, the result for this
simulation is `NULL`

## Details

For multiple simulations, only individual simulations are possible. For
single simulation, either individual or population simulations can be
performed.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Running an individual simulation
# Results is a list with one object `SimulationResults`
results <- runSimulations(sim)

# Creating custom simulation run options

simRunOptions <- SimulationRunOptions$new()
simRunOptions$numberOfCores <- 3
simRunOptions$showProgress <- TRUE

# Running a population simulation
popPath <- system.file("extdata", "pop.csv", package = "ospsuite")
population <- loadPopulation(popPath)
results <- runSimulations(sim, population, simulationRunOptions = simRunOptions)[[1]]

# Running multiple simulations in parallel
sim2 <- loadSimulation(simPath)
sim3 <- loadSimulation(simPath)

# Results is a list of `SimulationResults`
results <- runSimulations(list(sim, sim2, sim3))
```
