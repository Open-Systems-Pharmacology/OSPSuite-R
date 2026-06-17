# Load a project from a snapshot

Converts one or more snapshot files (`.json`) into project files and
writes them to an output directory.

Only PK-Sim projects (`.pksim5`) are supported for now. Support for MoBi
projects is planned.

## Usage

``` r
loadProjectFromSnapshot(..., output = ".", runSimulations = FALSE)
```

## Arguments

- ...:

  character strings, path to snapshot files (`.json`) or a directory
  containing snapshot files to convert.

- output:

  character string, path to the output directory where to write the
  converted project files.

- runSimulations:

  logical, whether to run the simulations during conversion (default =
  `FALSE`).

## Examples

``` r
if (FALSE) { # \dontrun{
loadProjectFromSnapshot("path/to/snapshot.json", output = "path/to/output")
} # }
```
