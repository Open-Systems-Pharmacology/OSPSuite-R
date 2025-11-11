# DataImporterConfiguration

Configuration of data import from excel or csv files. To be used with
\#TODO

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `DataImporterConfiguration`

## Active bindings

- `timeColumn`:

  Name of the column for time values

- `timeUnit`:

  If `isTimeUnitFromColumn` is `FALSE`, unit of the values in time
  column If `isTimeUnitFromColumn` is `TRUE`, name of the column with
  units of the values in time column.

- `isTimeUnitFromColumn`:

  If `TRUE`, units of the values in time column are defined in the
  column `timeUnit`. If `FALSE`, the unit is defined by the value of
  `timeUnit`.

- `measurementColumn`:

  Name of the column for measurement values

- `lloqColumn`:

  Name of the column for LLOQ values If the column name is not set
  (value `NULL`), LLOQ values will be imported from the measurement
  column if values are written in the form '\< xxx' (e.g., '\<0.001').
  Otherwise, the values will be imported from the specified column

- `measurementDimension`:

  If `isMeasurementUnitFromColumn` is `FALSE`, dimension of the values
  in measurement column If `isMeasurementUnitFromColumn` is `TRUE`, the
  dimension is guessed from the unit defined in the column
  `measurementUnit` during import process and `$measurementDimension` is
  `NULL`. When changing dimension, the unit is set to the base unit of
  this dimension.

- `measurementUnit`:

  If `isMeasurementUnitFromColumn` is `FALSE`, unit of the values in
  measurement column If `isMeasurementUnitFromColumn` is `TRUE`, name of
  the column with units of the values in measurement column

- `isMeasurementUnitFromColumn`:

  If `TRUE`, units of the values in measurement column are defined in
  the column `measurementUnit`. If `FALSE`, the unit is defined by the
  value of `measurementUnit`.

- `errorColumn`:

  Name of the column for measurement error values If no error column is
  defined, the value is `NULL`. Setting the value to `NULL` removes an
  existing error column.

- `errorUnit`:

  If `isMeasurementUnitFromColumn` is `FALSE`, unit of the values in the
  error column If `isMeasurementUnitFromColumn` is `TRUE`, name of the
  column with units of the values in error column If no error column is
  present, the value is `NULL`

- `errorType`:

  Type of the measurement error values. See enum `DataErrorType` for
  possible values If no error column is present, the value is `NULL`

- `groupingColumns`:

  Column names by which the data will be grouped

- `sheets`:

  Names of the sheets (list of strings) of the excel workbook for which
  the configuration will be applied.

- `namingPattern`:

  Regular expression used for naming of loaded data sets. Words between
  curly brackets (e.g. `{Group Id}`) will be replaced by the value in
  the corresponding column. Further keywords are `{Source}` for the file
  name and `{Sheet}` for sheet name.

## Methods

### Public methods

- [`DataImporterConfiguration$new()`](#method-DataImporterConfiguration-new)

- [`DataImporterConfiguration$saveConfiguration()`](#method-DataImporterConfiguration-saveConfiguration)

- [`DataImporterConfiguration$addGroupingColumn()`](#method-DataImporterConfiguration-addGroupingColumn)

- [`DataImporterConfiguration$removeGroupingColumn()`](#method-DataImporterConfiguration-removeGroupingColumn)

- [`DataImporterConfiguration$print()`](#method-DataImporterConfiguration-print)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-set)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    DataImporterConfiguration$new(netObject = NULL)

#### Arguments

- `netObject`:

  A `NetObject` with the reference to .NET DataImporterConfiguration
  object If `NULL` (default), an empty configuration with columns "Time"
  and "Measurement" is created.

#### Returns

A new `DataImporterConfiguration` object.

------------------------------------------------------------------------

### Method `saveConfiguration()`

Save configuration to a XML file that can be used in PK-Sim/MoBi

#### Usage

    DataImporterConfiguration$saveConfiguration(filePath)

#### Arguments

- `filePath`:

  Path (incl. file name) to the location where the configuration will be
  exported to.

------------------------------------------------------------------------

### Method `addGroupingColumn()`

Add a column for grouping the data sets

#### Usage

    DataImporterConfiguration$addGroupingColumn(column)

#### Arguments

- `column`:

  Name of the column

------------------------------------------------------------------------

### Method `removeGroupingColumn()`

Remove a column for grouping the data sets

#### Usage

    DataImporterConfiguration$removeGroupingColumn(column)

#### Arguments

- `column`:

  Name of the column

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    DataImporterConfiguration$print(...)

#### Arguments

- `...`:

  Rest arguments.
