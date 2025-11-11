# Convert datasets in `DataCombined` to common units

When multiple (observed and/or simulated) datasets are present in a data
frame, they are likely to have different units. This function helps to
convert them to a common unit specified by the user.

This is especially helpful while plotting since the quantities from
different datasets to be plotted on the X-and Y-axis need to have same
units to be meaningfully compared.

## Usage

``` r
convertUnits(dataCombined, xUnit = NULL, yUnit = NULL)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class.

- xUnit, yUnit:

  Target units for `xValues` and `yValues`, respectively. If not
  specified (`NULL`), first of the existing units in the respective
  columns (`xUnit` and `yUnit`) will be selected as the common unit. For
  available dimensions and units, see
  [`ospsuite::ospDimensions`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospDimensions.md)
  and
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospUnits.md),
  respectively.

## Value

A data frame with measurement columns transformed to have common units.

In the returned tibble data frame, the following columns will always be
present:

name - group - dataType - xValues - xDimension - xUnit - yValues -
yErrorValues - yDimension - yUnit - yErrorType - yErrorUnit - molWeight

Importantly, the `xUnit` and `yUnit` columns will have unique entries.

## Note

Molecular weight is **required** for the conversion between certain
dimensions (`Amount`, `Mass`, `Concentration (molar)`, and
`Concentration (mass)`). Therefore, if molecular weight is missing for
these dimension, the unit conversion will fail.

## See also

Other data-combined:
[`DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DataCombined.md),
[`calculateResiduals()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/calculateResiduals.md)

## Examples

``` r
# simulated data
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simResults <- runSimulations(sim)[[1]]
outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# observed data
obsData <- lapply(
  c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
  function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
)
names(obsData) <- lapply(obsData, function(x) x$name)


# Create a new instance of `DataCombined` class
myDataCombined <- DataCombined$new()

# Add simulated results
myDataCombined$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPath,
  groups = "Aciclovir PVB"
)

# Add observed data set
myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

convertUnits(
  myDataCombined,
  xUnit = ospUnits$Time$s,
  yUnit = ospUnits$`Concentration [mass]`$`µg/l`
)
#> # A tibble: 504 × 27
#>    IndividualId xValues name           yValues xDimension xUnit yDimension yUnit
#>           <int>   <dbl> <chr>            <dbl> <chr>      <chr> <chr>      <chr>
#>  1            0       0 Organism|Peri…      0  Time       s     Concentra… µg/l 
#>  2            0      60 Organism|Peri…    733. Time       s     Concentra… µg/l 
#>  3            0     120 Organism|Peri…   2050. Time       s     Concentra… µg/l 
#>  4            0     180 Organism|Peri…   3382. Time       s     Concentra… µg/l 
#>  5            0     240 Organism|Peri…   4668. Time       s     Concentra… µg/l 
#>  6            0     300 Organism|Peri…   5901. Time       s     Concentra… µg/l 
#>  7            0     360 Organism|Peri…   7077. Time       s     Concentra… µg/l 
#>  8            0     420 Organism|Peri…   8194. Time       s     Concentra… µg/l 
#>  9            0     480 Organism|Peri…   9249. Time       s     Concentra… µg/l 
#> 10            0     540 Organism|Peri…  10242. Time       s     Concentra… µg/l 
#> # ℹ 494 more rows
#> # ℹ 19 more variables: molWeight <dbl>, dataType <chr>, yErrorValues <dbl>,
#> #   yErrorType <chr>, yErrorUnit <chr>, lloq <dbl>, Source <chr>, File <chr>,
#> #   Sheet <chr>, Molecule <chr>, Species <chr>, Organ <chr>, Compartment <chr>,
#> #   `Study Id` <chr>, Gender <chr>, Dose <chr>, Route <chr>,
#> #   `Patient Id` <chr>, group <chr>
```
