# Residuals versus time scatter plot

Residuals versus time scatter plot

## Usage

``` r
plotResidualsVsTime(
  dataCombined,
  defaultPlotConfiguration = NULL,
  scaling = "lin"
)
```

## Arguments

- dataCombined:

  A single instance of `DataCombined` class containing both observed and
  simulated datasets to be compared.

- defaultPlotConfiguration:

  A `DefaultPlotConfiguration` object, which is an `R6` class object
  that defines plot properties.

- scaling:

  A character of length one specifying the scale type for residual. can
  be lin or log.

## See also

Other plotting:
[`DefaultPlotConfiguration`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DefaultPlotConfiguration.md),
[`plotIndividualTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotIndividualTimeProfile.md),
[`plotObservedVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotObservedVsSimulated.md),
[`plotPopulationTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotPopulationTimeProfile.md),
[`plotResidualsVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/plotResidualsVsSimulated.md)

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
plotResidualsVsTime(myDataCombined, scaling = "lin", defaultPlotConfiguration = myPlotConfiguration)

```
