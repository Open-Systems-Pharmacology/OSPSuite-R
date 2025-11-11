# Load `DataImporterConfiguration` from XML file.

Load `DataImporterConfiguration` from XML file.

## Usage

``` r
loadDataImporterConfiguration(configurationFilePath)
```

## Arguments

- configurationFilePath:

  Path to the XML file with stored configuration (e.g. created in PK-Sim
  or MoBi).

## Value

A new `DataImporterConfiguration` object to be used with
[`loadDataSetsFromExcel()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadDataSetsFromExcel.md).

## Examples

``` r
configurationFilePath <- system.file(
  "extdata", "dataImporterConfiguration.xml",
  package = "ospsuite"
)

importerConfiguration <- loadDataImporterConfiguration(configurationFilePath)

# Specifying which sheet to load
importerConfiguration$sheets <- "TestSheet_1"
xlsFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfiguration,
  importAllSheets = FALSE
)
```
