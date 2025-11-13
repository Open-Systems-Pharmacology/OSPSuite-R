# Observed versus predicted/simulated scatter plot

Observed versus predicted/simulated scatter plot

## Usage

``` r
plotObservedVsSimulated(
  dataCombined,
  defaultPlotConfiguration = NULL,
  foldDistance = NULL
)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class containing both observed and
  simulated datasets to be compared.

- defaultPlotConfiguration:

  A `DefaultPlotConfiguration` object, which is an `R6` class object
  that defines plot properties.

- foldDistance:

  A vector for plotting lines at required fold distances around the
  identity line (`x=y`). Set to NULL (default) to only draw identity
  line. Set to FALSE to not draw any lines. The vector can include only
  fold distance values `>1`. An `x`-fold distance is defined as all
  simulated values within the range between `x`-fold (depicted by the
  upper fold range line) and `1/x`-fold (depicted by the lower fold
  range line) of observed values. The identity line can be interpreted
  as the `1`-fold range.

## See also

Other plotting:
[`DefaultPlotConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DefaultPlotConfiguration.md),
[`plotIndividualTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotIndividualTimeProfile.md),
[`plotPopulationTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotPopulationTimeProfile.md),
[`plotResidualsVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsSimulated.md),
[`plotResidualsVsTime()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsTime.md)

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

# Create a new instance of `DefaultPlotConfiguration` class
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"

# plot
plotObservedVsSimulated(myDataCombined, myPlotConfiguration)
```
