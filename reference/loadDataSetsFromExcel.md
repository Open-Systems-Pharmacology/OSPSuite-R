# Load data sets from excel

Load data sets from excel

## Usage

``` r
loadDataSetsFromExcel(
  xlsFilePath,
  importerConfigurationOrPath,
  importAllSheets = FALSE,
  sheets = NULL
)
```

## Arguments

- xlsFilePath:

  Path to the excel file with the data

- importerConfigurationOrPath:

  An object of type `DataImporterConfiguration` that is valid for the
  excel file or a path to a XML file with stored configuration

- importAllSheets:

  **\[deprecated\]** If `FALSE` (default), only sheets specified in the
  `importerConfiguration` or in the `sheets` parameter will be loaded.
  If `TRUE`, an attempt to load all sheets is performed. If any sheet
  does not comply with the configuration, an error is thrown. When set
  to `TRUE`, this parameter takes priority over the `sheets` parameter
  and configuration sheets.

  **Deprecated**: Use `sheets = NULL` instead. This parameter will be
  removed in version 14.

- sheets:

  Character vector of sheet names to load, or `NULL` (default). If
  `NULL` and `importAllSheets` is `FALSE`, the sheets defined in the
  `importerConfiguration` will be used. If the configuration has no
  sheets defined and `sheets` is `NULL` and `importAllSheets` is
  `FALSE`, all sheets will be loaded. If a character vector is provided,
  only the specified sheets will be loaded, overriding any sheets
  defined in the `importerConfiguration` (unless
  `importAllSheets = TRUE`).

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

# When sheet is specified, it is automatically added to the configuration
importerConfiguration <- createImporterConfigurationForFile(
  xlsFilePath,
  sheet = "TestSheet_1"
)

dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfiguration
)

# Load specific sheets using the sheets parameter
dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfiguration,
  sheets = c("TestSheet_1", "TestSheet_2")
)

if (FALSE) { # \dontrun{
# Load all sheets by setting sheets to NULL and no sheets in configuration
importerConfiguration <- createImporterConfigurationForFile(xlsFilePath)
dataSets <- loadDataSetsFromExcel(
  xlsFilePath = xlsFilePath,
  importerConfigurationOrPath = importerConfiguration,
  sheets = NULL
)
} # }
```
