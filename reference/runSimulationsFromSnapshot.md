# Run Simulations From Snapshot Files

Run Simulations From Snapshot Files

## Usage

``` r
runSimulationsFromSnapshot(
  ...,
  output = ".",
  RunForAllOutputs = FALSE,
  exportCSV = TRUE,
  exportPKML = FALSE,
  exportJSON = FALSE,
  exportXML = FALSE
)
```

## Arguments

- ...:

  character strings, path to snapshot files or a directory containing
  snapshot files

- output:

  character string, path to the output directory where to write
  simulation results

- RunForAllOutputs:

  logical, whether to run the simulation for all outputs or only
  OutputSelections (default = FALSE)

- exportCSV:

  logical, whether to export the results as csv (default = TRUE)

- exportPKML:

  logical, whether to export the results as pkml (default = FALSE)

- exportJSON:

  logical, whether to export simulation results as json (default =
  FALSE)

- exportXML:

  logical, whether to export the results as xml (default = FALSE)

## Examples

``` r
if (FALSE) { # \dontrun{
runSimulationsFromSnapshot("path/to/my_snapshot.json", csv = TRUE, pkml = TRUE)
} # }
```
