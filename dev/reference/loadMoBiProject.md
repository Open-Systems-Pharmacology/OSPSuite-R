# Load a MoBi project

Load a MoBi project

## Usage

``` r
loadMoBiProject(filePath)
```

## Arguments

- filePath:

  Path of 'mbp3' MoBi project file to load.

## Value

An object of the `MoBiProject` type.

## Examples

``` r
if (FALSE) { # \dontrun{
projectPath <- system.file("extdata", "simple.mbp3", package = "ospsuite")

myProject <- loadMoBiProject(projectPath)
} # }
```
