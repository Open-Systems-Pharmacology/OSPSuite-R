# Calculate residuals for datasets in `DataCombined`

Computes residuals between observed and simulated datasets by
interpolating simulated values to observed time points and calculating
the difference according to the specified scaling method.

## Usage

``` r
calculateResiduals(dataCombined, scaling, xUnit = NULL, yUnit = NULL)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class containing both observed and
  simulated datasets to be compared.

- scaling:

  A character specifying the scaling method for residual calculation.
  Must be either `tlf::Scaling$lin` for linear residuals (simulated -
  observed) or `tlf::Scaling$log` for logarithmic residuals
  (log(simulated) - log(observed)).

- xUnit, yUnit:

  Target units for `xValues` and `yValues`, respectively. If not
  specified (`NULL`), the first existing unit in the respective columns
  will be selected as the common unit. For available dimensions and
  units, see
  [`ospsuite::ospDimensions`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospDimensions.md)
  and
  [`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ospUnits.md).

## Value

A tibble (data frame) containing paired observed-simulated data with
calculated residuals. The following columns will be present:

- group:

  Grouping identifier from the original DataCombined object

- name:

  Name of the observed dataset

- nameSimulated:

  Name of the paired simulated dataset

- xValues:

  X-axis values (typically time points) from observed data

- xUnit:

  Unit of x-values after harmonization

- xDimension:

  Dimension of x-values (e.g., "Time")

- yValuesObserved:

  Observed y-values at each x-point

- yValuesSimulated:

  Simulated y-values interpolated to observed x-points

- residualValues:

  Calculated residuals (method depends on scaling parameter)

- yUnit:

  Unit of y-values after harmonization

- yDimension:

  Dimension of y-values (e.g., "Concentration")

- yErrorValues:

  Error values from observed data (if available)

- yErrorType:

  Type of error (e.g., "SD", "SE")

- yErrorUnit:

  Unit of error values

- lloq:

  Lower limit of quantification (if available)

Returns `NULL` with a warning if no pairable datasets are found.

## Details

### Algorithm Overview

The function performs the following steps to calculate residuals:

1.  **Data Validation and Pairing**: For each group in the data, the
    function pairs observed datasets with simulated datasets. Any
    unpaired datasets (observed without corresponding simulated or vice
    versa) are removed.

2.  **Unit Harmonization**: All datasets are converted to common units
    (specified by `xUnit` and `yUnit` parameters) to ensure consistent
    calculations.

3.  **Interpolation**: For each observed-simulated pair, the function
    uses linear interpolation to estimate simulated values at the exact
    time points where observations exist:

    - With 2+ simulated points: Linear interpolation via
      [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)

    - With 1 simulated point: Direct matching for identical x-values
      only

    - With 0 simulated points: All residuals set to NA

4.  **Residual Calculation**: Residuals are computed based on the
    scaling method:

    - **Linear scaling** (`tlf::Scaling$lin`): \$\$residual =
      y\_{simulated} - y\_{observed}\$\$

    - **Logarithmic scaling** (`tlf::Scaling$log`): \$\$residual =
      \log(y\_{simulated}) - \log(y\_{observed})\$\$ Note: For log
      scaling, a small epsilon value is used to handle zero or near-zero
      values safely.

### Important Notes

- Residuals can only be computed when both observed and simulated data
  exist for the same group

- Interpolation does not extrapolate beyond the range of simulated data
  (returns NA for observed points outside simulated time range)

- NA residual values are automatically filtered from the output

- When multiple observed/simulated datasets exist in a group, all
  possible pairs are evaluated

## See also

Other data-combined:
[`DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataCombined.md),
[`convertUnits()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/convertUnits.md)

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
