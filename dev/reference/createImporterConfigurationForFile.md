# Create a `DataImporterConfiguration` for an XLS sheet

Create a `DataImporterConfiguration` for an XLS sheet

## Usage

``` r
createImporterConfigurationForFile(filePath, sheet = NULL)
```

## Arguments

- filePath:

  Path to XLS file

- sheet:

  optional - name of the sheet. If no sheet is specified, the first
  sheet of the XLS file is used.

## Value

`DataImporterConfiguration` object for XLS file to be used in
[`loadDataSetsFromExcel()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadDataSetsFromExcel.md).

## Details

The function tries to parse the structure of the excel sheet and creates
a default configuration for this sheet. It is advised to check the
configuration and adjust if necessary before using with
[`loadDataSetsFromExcel()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadDataSetsFromExcel.md).

## Examples

``` r
xlsFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
importerConfiguration <- createImporterConfigurationForFile(xlsFilePath)
importerConfiguration$sheets <- "TestSheet_1"

dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfiguration,
  importAllSheets = FALSE
)
```
