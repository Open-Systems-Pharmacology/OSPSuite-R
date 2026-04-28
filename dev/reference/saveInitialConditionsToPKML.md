# Save an Initial Conditions Building Block to pkml

Exports an `Initial Conditions` building block to a pkml file that can
be loaded by MoBi.

## Usage

``` r
saveInitialConditionsToPKML(initialConditionsBuildingBlock, filePath)
```

## Arguments

- initialConditionsBuildingBlock:

  A `BuildingBlock` object of type `Initial Conditions`.

- filePath:

  Path where the pkml file will be created. Must end with the `.pkml`
  extension.

## Value

`filePath`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
simulation <- loadSimulation("path/to/simulation.pkml")
icBB <- simulation$configuration$modules[[1]]$getInitialConditionsBBs()[[1]]
saveInitialConditionsToPKML(icBB, "InitialConditions.pkml")
} # }
```
