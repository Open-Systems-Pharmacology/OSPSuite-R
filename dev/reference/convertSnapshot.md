# Convert between snapshot and project formats

**\[deprecated\]**

`convertSnapshot()` is deprecated and will be removed in a future
release. Use
[`loadProjectFromSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadProjectFromSnapshot.md)
to convert a snapshot to a project, and
[`exportProjectToSnapshot()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/exportProjectToSnapshot.md)
to convert a project to a snapshot.

## Usage

``` r
convertSnapshot(..., format, output = ".", runSimulations = FALSE)
```

## Arguments

- ...:

  character strings, path to files or a directory containing files to
  convert

- format, :

  character string, target format either "snapshot" or "project".

- output:

  character string, path to the output directory where to write the
  converted files

- runSimulations:

  logical, whether to run simulations during conversion (default =
  FALSE). Only when converting from snapshot to project.
