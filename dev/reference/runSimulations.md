# Runs one or several simulations (individual and/or population)

Runs one or several simulations (individual and/or population)

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
  allowed when simulating one simulation, and applied for this run only
  (see Details). Alternatively, you can also pass the result of
  `createPopulation` directly. In this case, the population will be
  extracted. To run several population simulations at once, assign a
  population to each simulation with
  `simulation$population <- myPopulation` instead.

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

A list of simulations may mix individual and population simulations.
Whether a simulation is run as a population simulation is determined by
whether a population is assigned to it (see `simulation$population` and
`simulation$isPopulation`); a simulation loaded from a snapshot with
[`loadSimulationsFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulationsFromSnapshot.md)
may already carry one. Individual simulations are run in parallel and
population simulations sequentially.

There are two ways to run a population simulation:

- **Assign a population to the simulation** with
  `simulation$population <- myPopulation` and then call
  `runSimulations(simulation)`. Because the population travels with the
  simulation, this is the only way to run **several** population
  simulations in a single call - assign a population to each simulation
  and pass them all together.

- **Pass the `population` argument** (and optionally `agingData`). This
  is a convenience for the **single simulation** case only and cannot be
  combined with more than one simulation. The population is applied to
  the simulation for this run only and the simulation's original state
  is restored afterwards, so the simulation object you pass in is left
  unchanged.

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

# Results is a list of `SimulationResults`
results <- runSimulations(list(sim, sim2))
```
