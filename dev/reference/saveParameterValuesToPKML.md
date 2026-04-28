# Save a Parameter Values Building Block to pkml

Exports a `Parameter Values` building block to a pkml file that can be
loaded by MoBi.

## Usage

``` r
saveParameterValuesToPKML(parameterValuesBuildingBlock, filePath)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`.

- filePath:

  Path where the pkml file will be created. Must end with the `.pkml`
  extension.

## Value

`filePath`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
simulation <- loadSimulation("path/to/simulation.pkml")
pvBB <- simulation$configuration$modules[[1]]$getParameterValuesBBs()[[1]]
saveParameterValuesToPKML(pvBB, "ParameterValues.pkml")
} # }
```
