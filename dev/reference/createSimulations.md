# Create simulations from simulation configurations

Creates one simulation per entry of `simulationConfigurations`. All
simulations are created in parallel.

## Usage

``` r
createSimulations(
  simulationConfigurations,
  createAllProcessRateParameters = FALSE,
  showWarnings = FALSE,
  stopIfFails = FALSE
)
```

## Arguments

- simulationConfigurations:

  A named list of `SimulationConfiguration` objects. The names are used
  as the names of the created simulations and must be unique.

- createAllProcessRateParameters:

  If `TRUE`, process rate parameters will be created for all reactions
  and transport processes.

- showWarnings:

  If `TRUE`, warnings generated during simulation creation will be shown
  as R warnings. Default is `FALSE`.

- stopIfFails:

  If `TRUE`, an error is thrown as soon as one simulation could not be
  created. If `FALSE` (default), a warning is shown for every simulation
  that could not be created, and its entry in the returned list is
  `NULL`.

## Value

A named list of `Simulation` objects, one per entry of
`simulationConfigurations` and in the same order. The entry of a
simulation that could not be created is `NULL`.
