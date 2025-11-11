# Observed data

[ospsuite](https://github.com/open-systems-pharmacology/ospsuite-r)
offers a concept of storing and processing numerical x-y data in a
unified format by implementing a `DataSet` class. `DataSet` objects
standardize handling of (observed) data coming from different sources,
such as excel files using the data importer functionality of the OSPS,
loaded from `*.pkml`, or manually created. This vignette gives an
overview of the options to create `DataSet` objects and combine them
into grouped data sets using the
[`DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/articles/data-combined.md)
class.

## DataSet

A `DataSet` object stores numerical data pairs - typically time as x
values and measurement as y values - and optionally the measurement
error . All values have a *dimension* and a *unit* (see [Dimensions and
Units](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/articles/unit-conversion.md)
for more information). Furthermore, each `DataSet` must have a *name*.
When creating a `DataSet` from scratch (e.g. when the user wants to
manually input observed data), a name must be provided:

``` r
library(ospsuite)

# Create an empty data set
dataSet <- DataSet$new("My data set")
```

After creation, the `DataSet` does not hold any data. The default
dimension and unit for the x values is `Time` and `h`, respectively. The
default dimension and unit for the y values is `Concentration (mass)`
and `mg/l`, respectively. The dimension of the error values always
corresponds to the dimension of the y values, though the units may
differ.

Setting numerical values (or overwriting current values) is performed by
the `$setValues()` method:

``` r
dataSet$setValues(
  xValues = c(1, 2, 3, 4),
  yValues = c(0, 0.1, 0.6, 10),
  yErrorValues = c(0.001, 0.001, 0.1, 1)
)

print(dataSet)
#> <DataSet>
#>   • Name: My data set
#>   • X dimension: Time
#>   • X unit: h
#>   • Y dimension: Concentration (mass)
#>   • Y unit: mg/l
#>   • Error type: ArithmeticStdDev
#>   • Error unit: mg/l
#>   • Molecular weight: NULL
#>   • LLOQ: NULL
#> Meta data:
```

The user can change the dimensions and units of the values. After
changing the dimension, the unit is automatically set to the *base unit*
of the dimension. Changing the dimension or unit *does not* transform
the values.

``` r
# Print x, y, and error values
dataSet$xValues
#> [1] 1 2 3 4
dataSet$yValues
#> [1]  0.0  0.1  0.6 10.0
dataSet$yErrorValues
#> [1] 0.001 0.001 0.100 1.000

# Change the unit of x-values
dataSet$xUnit <- ospUnits$Time$min
# Print the x values - they did not change
dataSet$xValues
#> [1] 1 2 3 4

# Change dimension of y-values
dataSet$yDimension <- ospDimensions$Amount

print(dataSet)
#> <DataSet>
#>   • Name: My data set
#>   • X dimension: Time
#>   • X unit: min
#>   • Y dimension: Amount
#>   • Y unit: µmol
#>   • Error type: ArithmeticStdDev
#>   • Error unit: µmol
#>   • Molecular weight: NULL
#>   • LLOQ: NULL
#> Meta data:

# Change the units of y values and error values - they are now different!
dataSet$yUnit <- ospUnits$Amount$mol
dataSet$yErrorUnit <- ospUnits$Amount$pmol

print(dataSet)
#> <DataSet>
#>   • Name: My data set
#>   • X dimension: Time
#>   • X unit: min
#>   • Y dimension: Amount
#>   • Y unit: mol
#>   • Error type: ArithmeticStdDev
#>   • Error unit: pmol
#>   • Molecular weight: NULL
#>   • LLOQ: NULL
#> Meta data:
```

Two types of error values are supported - arithmetic error (default) and
geometric error, the latter being given in fraction. The user can change
the error type:

``` r
# Default error type is "ArithmeticStdDev"
dataSet$yErrorType
#> [1] "ArithmeticStdDev"

# Change error type to geometric
dataSet$yErrorType <- DataErrorType$GeometricStdDev
# Error unit is "Unitless" for dimension "Fraction".
dataSet$yErrorUnit
#> [1] ""

# Changing error type to arithmetic will set the dimension and unit of the error
# to the same dimension and unit as the y values
dataSet$yErrorType <- DataErrorType$ArithmeticStdDev

print(dataSet)
#> <DataSet>
#>   • Name: My data set
#>   • X dimension: Time
#>   • X unit: min
#>   • Y dimension: Amount
#>   • Y unit: mol
#>   • Error type: ArithmeticStdDev
#>   • Error unit: mol
#>   • Molecular weight: NULL
#>   • LLOQ: NULL
#> Meta data:
```

A `DataSet` can store any kind of text meta data as name-values pairs
and can be added by the `addMetaData()` method:

``` r
# Add new meta data entries
dataSet$addMetaData(
  name = "Molecule",
  value = "Aciclovir"
)
dataSet$addMetaData(
  name = "Organ",
  value = "Muscle"
)

# Print meta data of the DataSet
print(dataSet$metaData)
#> $Molecule
#> [1] "Aciclovir"
#> 
#> $Organ
#> [1] "Muscle"
```

A `DataSet` or multiple `DataSet`s can be converted to `data.frame` (or
`tibble`) to be processed in downstream analysis and visualization
workflows:

``` r
# Create a second data set
dataSet2 <- DataSet$new(name = "Second data set")
dataSet2$setValues(
  xValues = c(1, 2, 3, 4, 5),
  yValues = c(1, 0, 5, 8, 0.1)
)

# Convert data sets to a tibble
myTibble <- dataSetToTibble(dataSets = c(dataSet, dataSet2))

print(myTibble)
#> # A tibble: 9 × 14
#>   name            xValues yValues yErrorValues xDimension xUnit yDimension yUnit
#>   <chr>             <dbl>   <dbl>        <dbl> <chr>      <chr> <chr>      <chr>
#> 1 My data set           1   0         0.001000 Time       min   Amount     mol  
#> 2 My data set           2   0.1       0.001000 Time       min   Amount     mol  
#> 3 My data set           3   0.6       0.1      Time       min   Amount     mol  
#> 4 My data set           4  10         1        Time       min   Amount     mol  
#> 5 Second data set       1   1.000    NA        Time       h     Concentra… mg/l 
#> 6 Second data set       2   0        NA        Time       h     Concentra… mg/l 
#> 7 Second data set       3   5.00     NA        Time       h     Concentra… mg/l 
#> 8 Second data set       4   8.00     NA        Time       h     Concentra… mg/l 
#> 9 Second data set       5   0.100    NA        Time       h     Concentra… mg/l 
#> # ℹ 6 more variables: yErrorType <chr>, yErrorUnit <chr>, molWeight <dbl>,
#> #   lloq <dbl>, Molecule <chr>, Organ <chr>
```

## Importing data

Creating `DataSet` objects from scratch is a rather advanced use case.
Typically, observed data are loaded either from `*.pkml` files exported
from PK-Sim or MoBi, or imported from Excel files. The function
[`loadDataSetFromPKML()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadDataSetFromPKML.md)
loads data from the `*.pkml` file. Complementary to this function is the
function
[`saveDataSetToPKML()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/saveDataSetToPKML.md)
that allows to export any `DataSet` to a `*.pkml` that can be loaded
e.g. in MoBi.

``` r
# Load a data set from PKML
filePath <- system.file("extdata", "ObsDataAciclovir_1.pkml", package = "ospsuite")

dataSet <- loadDataSetFromPKML(filePath = filePath)

print(dataSet)
#> <DataSet>
#>   • Name: Vergin 1995.Iv
#>   • X dimension: Time
#>   • X unit: h
#>   • Y dimension: Concentration (mass)
#>   • Y unit: mg/l
#>   • Error type: ArithmeticStdDev
#>   • Error unit: mg/l
#>   • Molecular weight: 225.21
#>   • LLOQ: NULL
#> Meta data:
#>   • Source: X:\Orga\BTS-TD\ET\TP CSB\Projects\Internal
#>   Projects\MagenDarm\TestSubstanzen\Acyclovir\Rohdaten_Acyclovir.xls.Vergin
#>   1995 250 mg iv
#>   • File: Rohdaten_Acyclovir
#>   • Sheet: Vergin 1995 250 mg iv
#>   • Molecule: Aciclovir
#>   • Species: Human
#>   • Organ: Peripheral Venous Blood
#>   • Compartment: Plasma
#>   • Study Id: Vergin 1995
#>   • Gender: Undefined
#>   • Dose: 250 mg
#>   • Route: IV
#>   • Patient Id: Iv
```

Another (and probably the most important) way to create `DataSet`
objects is by importing data from excel files. The function
[`loadDataSetsFromExcel()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadDataSetsFromExcel.md)
utilizes the data import functionality implemented in PK-Sim and MoBi
and returns a set of `DataSet` objects. For description of the supported
file formats and configurations, please refer to the [OSPS
documentation](https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/import-edit-observed-data).

Loading observed data from an Excel sheet requires an
`ImporterConfiguration`. The configuration describes mapping of excel
sheet columns to numerical data (e.g.  which column contains the x
values) or meta data (e.g., description of the applied dose). One way to
obtain such configuration is to create it in PK-Sim or MoBi, save it (as
an `*.xml`) file, and load it in R with the
[`loadDataImporterConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadDataImporterConfiguration.md)
function:

``` r
# Load a configuration from xml file
filePath <- system.file("extdata", "dataImporterConfiguration.xml", package = "ospsuite")
importerConfiguration <- loadDataImporterConfiguration(configurationFilePath = filePath)

print(importerConfiguration)
#> <DataImporterConfiguration>
#>   • Time column: Time [h]
#>   • Time unit: h
#>   • Time unit from column: FALSE
#>   • Measurement column: Concentration (mass)[ng/ml]
#>   • Measurement unit: ng/ml
#>   • Measurement unit from column: FALSE
#>   • LLOQ column: NULL
#>   • Error column: Error [ng/ml]
#>   • Error type: ArithmeticStdDev
#>   • Error unit: ng/ml
#>   • Grouping columns: Study Id, Organ, Compartment, Species, Gender, Molecule,
#>   Route, MW, Patient Id, Dose [unit]
#>   • Sheets: <empty vector>
#>   • Naming pattern: {Source}.{Sheet}.{Study
#>   Id}.{Organ}.{Compartment}.{Species}.{Gender}.{Molecule}.{Route}.{Molecular
#>   Weight}.{Subject Id}.{Dose}
```

A data importer configuration can also be created from scratch and has
to be manually populated by the user. Alternatively, the user can let
the software “guess” the configuration for a given excel sheet:

``` r
# Excel file
excelFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
sheetName <- "TestSheet_1"

# Create importer configuration for the excel sheet
importerConfiguration_guessed <- createImporterConfigurationForFile(
  filePath = excelFilePath,
  sheet = sheetName
)

print(importerConfiguration)
#> <DataImporterConfiguration>
#>   • Time column: Time [h]
#>   • Time unit: h
#>   • Time unit from column: FALSE
#>   • Measurement column: Concentration (mass)[ng/ml]
#>   • Measurement unit: ng/ml
#>   • Measurement unit from column: FALSE
#>   • LLOQ column: NULL
#>   • Error column: Error [ng/ml]
#>   • Error type: ArithmeticStdDev
#>   • Error unit: ng/ml
#>   • Grouping columns: Study Id, Organ, Compartment, Species, Gender, Molecule,
#>   Route, MW, Patient Id, Dose [unit]
#>   • Sheets: <empty vector>
#>   • Naming pattern: {Source}.{Sheet}.{Study
#>   Id}.{Organ}.{Compartment}.{Species}.{Gender}.{Molecule}.{Route}.{Molecular
#>   Weight}.{Subject Id}.{Dose}
```

It is important to manually check the created configuration, as the
automated configuration recognition cannot cover all possible cases.

If only specific sheets from the excel file should be imported, they can
be specified in the `ImporterConfiguration`. The following example loads
the sheets `TestSheet_1` and `TestSheet_1_withMW`:

``` r
# Excel file
excelFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
sheetName <- "TestSheet_1"

# Create importer configuration for the excel sheet
importerConfiguration_guessed <- createImporterConfigurationForFile(
  filePath = excelFilePath,
  sheet = sheetName
)
# Add sheet names to the configuration
importerConfiguration_guessed$sheets <- c("TestSheet_1", "TestSheet_1_withMW")

# Load data
dataSets <- loadDataSetsFromExcel(
  xlsFilePath = excelFilePath,
  importerConfigurationOrPath = importerConfiguration_guessed
)
```
