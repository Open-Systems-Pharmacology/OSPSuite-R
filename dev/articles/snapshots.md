# Working with snapshots

A *snapshot* is a JSON representation of a PK-Sim or MoBi project.
Snapshots are human-readable, diff-friendly, and
version-control-tractable, which makes them a convenient exchange format
for projects. Read more about OSP snapshots in the
[documentation](https://docs.open-systems-pharmacology.org/working-with-pk-sim/pk-sim-documentation/importing-exporting-project-data-models#exporting-project-to-snapshot-loading-project-from-snapshot).
[ospsuite](https://github.com/open-systems-pharmacology/ospsuite-r)
exposes helpers to load simulations from a snapshot, run simulations
stored in a snapshot, and convert between the snapshot and project
formats.

> **Note**
>
> The helpers described in this vignette support **PK-Sim snapshots
> only**; MoBi snapshots are not yet supported.

``` r

library(ospsuite)

# This vignette uses the example snapshot shipped with the package
snapshotPath <- system.file(
  "extdata",
  "test_snapshot.json",
  package = "ospsuite"
)
```

## Loading simulations from a snapshot

[`loadSimulationsFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulationsFromSnapshot.md)
creates the simulations stored in a snapshot file and returns them as a
named list of `Simulation` objects.

By default, every simulation in the snapshot is loaded:

``` r

simulations <- loadSimulationsFromSnapshot(snapshotPath)

# Names of the loaded simulations
allNames <- names(simulations)
allNames
```

    ## [1] "Simulation - IV + Weibull - Default tolerance"
    ## [2] "Simulation - IV only - Default tolerance"

When you only need specific simulations, pass their names via
`simulationNames`. The match is case-sensitive:

``` r

oneSimulation <- loadSimulationsFromSnapshot(
  snapshotPath,
  simulationNames = allNames[1]
)

oneSimulation[[1]]$name
```

    ## [1] "Simulation - IV + Weibull - Default tolerance"

By default, requesting a name that is not in the snapshot raises an
error. Set `ignoreIfNotFound = TRUE` to instead get a `NULL` entry for
each missing name, keeping the order of the requested names:

``` r

loadSimulationsFromSnapshot(
  snapshotPath,
  simulationNames = c(allNames[1], "Does not exist"),
  ignoreIfNotFound = TRUE
)
```

    ## $`Simulation - IV + Weibull - Default tolerance`
    ## <Simulation>
    ##   • Name: Simulation - IV + Weibull - Default tolerance
    ## 
    ## $`Does not exist`
    ## NULL

Once loaded, a simulation can be used like any other:

``` r

simulation <- simulations[[1]]

results <- runSimulations(simulation)[[1]]
results
```

    ## <SimulationResults>
    ##   • Number of individuals: 1
    ## For paths:
    ##   • Organism|VenousBlood|Plasma|Generic_compound|Concentration in container
    ##   • Organism|Skin|Intracellular|Generic_compound|Concentration in container

## Running simulations from a snapshot

[`runSimulationsFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/runSimulationsFromSnapshot.md)
runs every simulation contained in one or more snapshot files (or
directories of snapshot files) and writes the results to an output
directory. Unlike
[`loadSimulationsFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadSimulationsFromSnapshot.md),
it does not return the simulations to R; instead it exports the results
to disk in the requested formats.

``` r

outputDir <- file.path(tempdir(), "snapshot-results")
dir.create(outputDir, showWarnings = FALSE)

runSimulationsFromSnapshot(
  snapshotPath,
  output = outputDir,
  exportCSV = TRUE,
  exportPKML = TRUE
)
```

    ## ℹ Running simulations from 1 snapshot

    ## ✔ Simulations completed

    ## 

``` r

list.files(outputDir)
```

    ## [1] "snapshot-results.csv"                                                   
    ## [2] "test_snapshot-Simulation - IV + Weibull - Default tolerance-Results.csv"
    ## [3] "test_snapshot-Simulation - IV + Weibull - Default tolerance.pkml"       
    ## [4] "test_snapshot-Simulation - IV only - Default tolerance-Results.csv"     
    ## [5] "test_snapshot-Simulation - IV only - Default tolerance.pkml"

The export formats are controlled independently:

- `exportCSV` (default `TRUE`) — results as CSV.
- `exportPKML` (default `FALSE`) — simulations as PKML.
- `exportJSON` (default `FALSE`) — results as JSON.
- `exportXML` (default `FALSE`) — simulations as XML.

Set `RunForAllOutputs = TRUE` (default: `FALSE`) to compute all model
outputs instead of only the output selections defined in the snapshot.

You can also point the function at a directory and it will process every
snapshot in it:

``` r

snapshotDir <- file.path(tempdir(), "snapshots")
dir.create(snapshotDir, showWarnings = FALSE)
file.copy(snapshotPath, snapshotDir, overwrite = TRUE)
```

    ## [1] TRUE

``` r

runSimulationsFromSnapshot(snapshotDir, output = outputDir)
```

    ## ℹ Running simulations from 1 snapshot

    ## ✔ Simulations completed

    ## 

## Converting between snapshot and project formats

Snapshots and PK-Sim project files (`.pksim5`) can be converted into
each other. Use
[`loadProjectFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadProjectFromSnapshot.md)
to build a project from a snapshot, and
[`exportProjectToSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/exportProjectToSnapshot.md)
to write a project back out as a snapshot.

Convert a snapshot to a PK-Sim project:

``` r

projectDir <- file.path(tempdir(), "project")
dir.create(projectDir, showWarnings = FALSE)

loadProjectFromSnapshot(snapshotPath, output = projectDir)
```

    ## ℹ Converting 1 file to project format

    ## ✔ Conversion completed

    ## 

``` r

projectFile <- list.files(projectDir, pattern = "\\.pksim5$", full.names = TRUE)
projectFile
```

    ## [1] "/tmp/RtmpsPQsZj/project/test_snapshot.pksim5"

Set `runSimulations = TRUE` to also run the simulations during this
conversion.

And convert a project back to a snapshot:

``` r

snapshotOut <- file.path(tempdir(), "snapshot")
dir.create(snapshotOut, showWarnings = FALSE)

exportProjectToSnapshot(projectFile, output = snapshotOut)
```

    ## ℹ Converting 1 file to snapshot format

    ## ✔ Conversion completed

    ## 

``` r

list.files(snapshotOut)
```

    ## [1] "test_snapshot.json"

As with
[`runSimulationsFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/runSimulationsFromSnapshot.md),
both functions accept several files or directories at once.

> **Note**
>
> [`convertSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/convertSnapshot.md)
> is deprecated in favour of
> [`loadProjectFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadProjectFromSnapshot.md)
> and
> [`exportProjectToSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/exportProjectToSnapshot.md).
