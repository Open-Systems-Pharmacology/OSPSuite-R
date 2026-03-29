# Extract Unique Elements of type 'Entity'

Extract Unique Elements of type 'Entity'

## Usage

``` r
uniqueEntities(entities, compareBy = CompareBy$id)
```

## Arguments

- entities:

  List of objects of type 'Entity'

- compareBy:

  A string defining the property that is compared by. Can take values
  'id', 'name', and 'path'. Default is 'id'.

## Value

List of entities that are unique for the property defined by the
argument 'compareBy'.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

parameters <- c(
  getParameter(toPathString(c("Organism", "Liver", "Volume")), sim),
  getParameter(toPathString(c("Organism", "Liver", "Volume")), sim),
  getParameter(toPathString(c("Organism", "TableParameter")), sim)
)

# Return a list containing the two parameters 'Volume' and 'Weight (tissue)'
uniqueEntities(parameters, CompareBy$id)
#> [[1]]
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 1.00 [1/min]
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 1, restartSolver= FALSE
#>   x= 10, y= 2, restartSolver= FALSE
#>   x= 30, y= 3, restartSolver= FALSE
#>   x= 40, y= 4, restartSolver= FALSE
#>   • Value overrides formula: FALSE
#> 
#> [[2]]
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|Liver|Volume
#>   • Value: 10.00 [l]
#> 
#> ── Formula ──
#> 
#>   • isConstant: TRUE
#> 
```
