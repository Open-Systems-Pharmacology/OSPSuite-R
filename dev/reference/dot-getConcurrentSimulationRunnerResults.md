# Get SimulationResults from ConcurrentSimulationRunner

Get SimulationResults from ConcurrentSimulationRunner

## Usage

``` r
.getConcurrentSimulationRunnerResults(
  results,
  resultsIdSimulationIdMap,
  simulationIdSimulationMap,
  silentMode,
  stopIfFails
)
```

## Arguments

- results:

  .NET object created by `RunConcurrently()`

- resultsIdSimulationIdMap:

  Map of results ids as keys with values being the ids of simulations
  the respective batch was created with. The order of IDs is as they
  were added to the batch.

- simulationIdSimulationMap:

  A named list of simulation ids as keys and simulation objects as
  values to the id of a result

- silentMode:

  If `TRUE`, no warnings are displayed if a simulation fails. Default is
  `FALSE`. Has no effect if `stopIfFails` is `TRUE`.

- stopIfFails:

  Whether to stop the execution if one of the simulations failed.
  Default is `FALSE`.

## Value

A named list of `SimulationResults` objects with the names being the ids
of simulations or simulation-batch values pairs they were produced by

## Details

Create a list of `SimulationResults`-objects from the results of a
`ConcurrentSimulationRunner`
