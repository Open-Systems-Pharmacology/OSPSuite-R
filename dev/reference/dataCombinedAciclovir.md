# dataCombinedAciclovir

Example `DataCombined` object for Aciclovir created from the code chunk
below and that is re-used throughout the package examples and
documentation

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
    dataCombinedAciclovir <- DataCombined$new()

    # Add simulated results
    dataCombinedAciclovir$addSimulationResults(
      simulationResults = simResults,
      quantitiesOrPaths = outputPath,
      groups = "Aciclovir PVB"
    )

    # Add observed data set
    dataCombinedAciclovir$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

## Usage

``` r
dataCombinedAciclovir
```

## Format

### `dataCombinedAciclovir`

A `DataCombined` object with 2 sets

- simulated:

  Simulated Aciclovir data

- observed:

  Observed data from Vergin 1995.Iv

## Source

[Working with DataCombined
class](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/articles/data-combined.md)
