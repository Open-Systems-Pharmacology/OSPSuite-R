# Calculate residuals for datasets in `DataCombined`

Calculate residuals for datasets in `DataCombined`

## Usage

``` r
calculateResiduals(dataCombined, scaling, xUnit = NULL, yUnit = NULL)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class.

- scaling:

  A character specifying scale: either `tlf::Scaling$lin` (linear) or
  `tlf::Scaling$log` (logarithmic).

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

In the returned tibble data frame, the following columns will always be
present:

xValues - xUnit - xDimension - yValuesObserved - yUnit - yDimension -
yErrorValues - yErrorType - yErrorUnit - yValuesSimulated -
residualValues

## Details

To compute residuals, for every simulated dataset in a given group,
there should also be a corresponding observed dataset. If this is not
the case, the corresponding observed or simulated datasets will be
removed.

When multiple (observed and/or simulated) datasets are present in
`DataCombined`, they are likely to have different units. The `xUnit` and
`yUnit` arguments help you specify a common unit to convert them to.

## See also

Other data-combined:
[`DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DataCombined.md),
[`convertUnits()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/convertUnits.md)

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

calculateResiduals(myDataCombined, scaling = tlf::Scaling$lin)
#> # A tibble: 13 × 15
#>    group      name  nameSimulated xValues xUnit xDimension yValuesObserved yUnit
#>    <chr>      <chr> <chr>           <dbl> <chr> <chr>                <dbl> <chr>
#>  1 Aciclovir… Verg… Organism|Per…    13.4 min   Time                35.0   µmol…
#>  2 Aciclovir… Verg… Organism|Per…    29.1 min   Time                20.0   µmol…
#>  3 Aciclovir… Verg… Organism|Per…    44.7 min   Time                14.1   µmol…
#>  4 Aciclovir… Verg… Organism|Per…    58.1 min   Time                11.0   µmol…
#>  5 Aciclovir… Verg… Organism|Per…    87.2 min   Time                 7.51  µmol…
#>  6 Aciclovir… Verg… Organism|Per…   119.  min   Time                 5.88  µmol…
#>  7 Aciclovir… Verg… Organism|Per…   179.  min   Time                 4.03  µmol…
#>  8 Aciclovir… Verg… Organism|Per…   239.  min   Time                 3.05  µmol…
#>  9 Aciclovir… Verg… Organism|Per…   360   min   Time                 1.63  µmol…
#> 10 Aciclovir… Verg… Organism|Per…   541.  min   Time                 0.871 µmol…
#> 11 Aciclovir… Verg… Organism|Per…   720   min   Time                 0.544 µmol…
#> 12 Aciclovir… Verg… Organism|Per…   901.  min   Time                 0.435 µmol…
#> 13 Aciclovir… Verg… Organism|Per…  1440   min   Time                 0.326 µmol…
#> # ℹ 7 more variables: yDimension <chr>, yErrorValues <dbl>, yErrorType <chr>,
#> #   yErrorUnit <chr>, lloq <dbl>, yValuesSimulated <dbl>, residualValues <dbl>
```
