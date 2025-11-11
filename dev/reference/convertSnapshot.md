# Convert between snapshot and project formats

Convert between snapshot and project formats

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

## Examples

``` r
if (FALSE) { # \dontrun{
convertSnapshot("path/to/snapshot.json", format = "project")
convertSnapshot("path/to/project.pksim5", format = "snapshot")
} # }
```
