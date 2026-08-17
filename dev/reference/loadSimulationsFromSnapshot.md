# Load simulations from a snapshot file

Loads the simulations stored in a snapshot file and returns them as a
list of
[Simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Simulation.md)
objects. By default every simulation in the snapshot is loaded. When
`simulationNames` are supplied, only the simulations whose name matches
(case-sensitive) are returned. By default, an error is thrown if any of
the requested names is not present in the snapshot. Set
`ignoreIfNotFound = TRUE` to instead return `NULL` for the names that
were not found.

## Usage

``` r
loadSimulationsFromSnapshot(
  snapshotFile,
  simulationNames = NULL,
  ignoreIfNotFound = FALSE
)
```

## Arguments

- snapshotFile:

  Character string, path to the snapshot file (`.json`).

- simulationNames:

  Optional character vector of simulation names to load. If `NULL`
  (default), all simulations in the snapshot are loaded.

- ignoreIfNotFound:

  Logical. If `FALSE` (default), an error is thrown when any of the
  requested `simulationNames` is not found in the snapshot. If `TRUE`,
  missing names are returned as `NULL` entries instead. Has no effect
  when `simulationNames` is `NULL`.

## Value

A named list of
[Simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Simulation.md)
objects, with names being the simulation names. When `simulationNames`
is supplied, the returned list keeps the order of the requested names;
with `ignoreIfNotFound = TRUE`, entries for names that were not found
are `NULL`.

## Examples

``` r
snapshotPath <- system.file("extdata", "test_snapshot.json", package = "ospsuite")

# Load every simulation from a snapshot
simulations <- loadSimulationsFromSnapshot(snapshotPath)

# Load only a specific simulation by name
firstName <- simulations[[1]]$name
oneSimulation <- loadSimulationsFromSnapshot(
  snapshotPath,
  simulationNames = firstName
)
```
