# Convert project files to snapshots

Converts one or more project files into snapshot files (`.json`) and
writes them to an output directory.

PK-Sim (`.pksim5`) and MoBi (`.mbp3`) projects are both supported. The
application is detected from the file extension; use `application` to
force the choice.

## Usage

``` r
projectToSnapshot(..., output = ".", application = NULL)
```

## Arguments

- ...:

  character strings, path to project files (`.pksim5` or `.mbp3`) or a
  directory containing project files to convert.

- output:

  character string, path to the output directory where to write the
  converted snapshot files.

- application:

  character string, `"PK-Sim"` or `"MoBi"`, the application the project
  files belong to. If `NULL` (default), it is detected for each project
  separately.

## Examples

``` r
if (FALSE) { # \dontrun{
projectToSnapshot("path/to/project.pksim5", output = "path/to/output")
projectToSnapshot("path/to/project.mbp3", output = "path/to/output")
} # }
```
