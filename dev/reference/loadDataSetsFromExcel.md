# Load data sets from excel

Load data sets from excel

## Usage

``` r
loadDataSetsFromExcel(
  xlsFilePath,
  importerConfigurationOrPath,
  importAllSheets = FALSE
)
```

## Arguments

- xlsFilePath:

  Path to the excel file with the data

- importerConfigurationOrPath:

  An object of type `DataImporterConfiguration` that is valid for the
  excel file or a path to a XML file with stored configuration

- importAllSheets:

  If `FALSE` (default), only sheets specified in the
  `importerConfiguration` will be loaded. If `TRUE`, an attempt to load
  all sheets is performed. If any sheet does not comply with the
  configuration, an error is thrown.

## Value

A named set of `DataSet` objects. The naming is defined by the property
`importerConfiguration$namingPattern`.

## Details

Load observed data from an excel file using an importer configuration

## Examples

``` r
xlsFilePath <- system.file(
  "extdata", "CompiledDataSet.xlsx",
  package = "ospsuite"
)

importerConfiguration <- createImporterConfigurationForFile(xlsFilePath)
importerConfiguration$sheets <- "TestSheet_1"

dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfiguration,
  importAllSheets = FALSE
)

importerConfigurationFilePath <- system.file(
  "extdata", "dataImporterConfiguration.xml",
  package = "ospsuite"
)

dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfigurationFilePath,
  importAllSheets = FALSE
)
```
