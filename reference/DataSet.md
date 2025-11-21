# DataSet

A class for storage of numerical x- and y-value pairs and optional error
for y-values.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `DataSet`

## Active bindings

- `name`:

  The name of the DataSet

- `dataRepository`:

  The underlying DataRepository object

- `xDimension`:

  Dimension in which the xValues are defined

- `xUnit`:

  Unit in which the xValues are defined

- `xValues`:

  Values stored in the xUnit. This field is read-only. Use
  `$setValues()` to change the values.

- `yDimension`:

  Dimension in which the yValues are defined

- `yUnit`:

  Unit in which the yValues are defined

- `yValues`:

  Values stored in the yUnit. This field is read-only. Use
  `$setValues()` to change the values.

- `yErrorType`:

  Type of the error - geometric or arithmetic. When changing from
  arithmetic to geometric error, the values are considered in as
  fraction (1 = 100%). When changing from geometric to arithmetic, the
  values are set to the same unit as `yErrorUnit`. In case no yError is
  defined, the value is `NULL` and cannot be changed

- `yErrorUnit`:

  Unit in which the yErrorValues are defined. For arithmetic error, the
  unit must be valid for `yDimension`. For geometric error, the unit
  must be valid for `Dimensionless`. In case no yError is defined, the
  value is `NULL` and cannot be changed

- `yErrorValues`:

  Values of error stored in the yErrorUnit unit. This field is
  read-only. Use `$setValues()` to change the values. In case no yError
  is defined, the value is `NULL` and cannot be changed. Use
  `$setValues()` to change the values.

- `molWeight`:

  Molecular weight of the yValues in g/mol

- `LLOQ`:

  Lower Limit Of Quantification. Value in yUnit associated with the
  yValues

- `metaData`:

  Returns a named list of meta data defined for the data set.

## Methods

### Public methods

- [`DataSet$new()`](#method-DataSet-new)

- [`DataSet$addMetaData()`](#method-DataSet-addMetaData)

- [`DataSet$removeMetaData()`](#method-DataSet-removeMetaData)

- [`DataSet$setValues()`](#method-DataSet-setValues)

- [`DataSet$print()`](#method-DataSet-print)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-set)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class. Either create a `DataSet` from a
`DataRepository` (e.g. loaded from a PKML) or an empty `DataSet`. In
case of an empty `DataSet`, a `name` must be provided.

#### Usage

    DataSet$new(name = NULL, dataRepository = NULL)

#### Arguments

- `name`:

  Name of the `DataSet` if created from scratch (no `dataRepository`)
  provided. Ignored if `dataRepository` is not `NULL`.

- `dataRepository`:

  Instance of the `DataRepository` object to wrap. If `NULL`, an empty
  `DataRepository` is created.

#### Returns

A new `DataSet` object.

------------------------------------------------------------------------

### Method `addMetaData()`

Adds a new entry to meta data list or changes its value if the name is
already present.

#### Usage

    DataSet$addMetaData(name, value)

#### Arguments

- `name`:

  Name of new meta data list entry

- `value`:

  Value of new meta data list entry

------------------------------------------------------------------------

### Method `removeMetaData()`

Removes the meta data entry in the list if one is defined with this name

#### Usage

    DataSet$removeMetaData(name)

#### Arguments

- `name`:

  Name of meta data entry to delete

------------------------------------------------------------------------

### Method `setValues()`

Sets the xValues, yValues, and (optionally) yErrorValues into the
dataSet. Note: xValues, yValues and yErrorValues must have the same
length

#### Usage

    DataSet$setValues(xValues, yValues, yErrorValues = NULL)

#### Arguments

- `xValues`:

  xValues to use

- `yValues`:

  yValues to use

- `yErrorValues`:

  Optional error values associated with yValues

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    DataSet$print(...)

#### Arguments

- `...`:

  Rest arguments.
