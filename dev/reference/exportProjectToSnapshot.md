# Export a project to a snapshot

Converts one or more project files into snapshot files (`.json`) and
writes them to an output directory.

Only PK-Sim projects (`.pksim5`) are supported for now. Support for MoBi
projects is planned.

## Usage

``` r
exportProjectToSnapshot(..., output = ".")
```

## Arguments

- ...:

  character strings, path to project files (`.pksim5`) or a directory
  containing project files to convert.

- output:

  character string, path to the output directory where to write the
  converted snapshot files.

## Examples

``` r
if (FALSE) { # \dontrun{
exportProjectToSnapshot("path/to/project.pksim5", output = "path/to/output")
} # }
```
