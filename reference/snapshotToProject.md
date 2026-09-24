# Convert snapshots to project files

Converts one or more snapshot files (`.json`) into project files and
writes them to an output directory.

PK-Sim (`.pksim5`) and MoBi (`.mbp3`) projects are both supported. The
application that wrote a snapshot is detected from the snapshot itself;
use `application` to force the choice.

## Usage

``` r
snapshotToProject(
  ...,
  output = ".",
  runSimulations = FALSE,
  application = NULL
)
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

- application:

  character string, `"PK-Sim"` or `"MoBi"`, the application whose
  project files are written. If `NULL` (default), it is detected for
  each snapshot separately.

## Examples

``` r
if (FALSE) { # \dontrun{
snapshotToProject("path/to/snapshot.json", output = "path/to/output")
} # }
```
