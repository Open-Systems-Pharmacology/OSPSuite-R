# Formula

A formula of the model (Typically related to a `Quantity` such as a
parameter)

A table formula of the model (Typically related to a `Quantity` such as
a parameter)

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `Formula`

## Active bindings

- `isTable`:

  Is this a table formula (Read-Only)

- `isTableWithOffSet`:

  Is this a table formula with Offset (Read-Only)

- `isTableWithXArgument`:

  Is this a table formula with xArgs (typically time, or pH) (Read-Only)

- `isConstant`:

  Is this a constant formula (Read-Only)

- `isExplicit`:

  Is this an explicit formula (Read-Only)

- `isDistributed`:

  Is this a distributed formula (Read-Only)

- `dimension`:

  The dimension in which the quantity is defined (Read-Only)

- `formulaString`:

  Returns the formula as a string for an `ExplicitFormula` or `NULL`
  otherwise (Read-Only).

## Methods

### Public methods

- [`Formula$print()`](#method-Formula-print)

- [`Formula$printFormula()`](#method-Formula-printFormula)

Inherited methods

- [`rSharp::NetObject$.printClass()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printClass)
- [`rSharp::NetObject$.printLine()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printLine)
- [`rSharp::NetObject$call()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-call)
- [`rSharp::NetObject$get()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-get)
- [`rSharp::NetObject$getFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getFields)
- [`rSharp::NetObject$getMemberSignature()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMethods)
- [`rSharp::NetObject$getProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getProperties)
- [`rSharp::NetObject$getStaticFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticProperties)
- [`rSharp::NetObject$set()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-set)
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.html#method-initialize)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Formula$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `printFormula()`

Print the formula to the console without the name of the class

#### Usage

    Formula$printFormula()

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `ospsuite::Formula` -\> `TableFormula`

## Active bindings

- `allPoints`:

  Returns all points defined in the table formula for a `TableFormula`
  or `NULL` otherwise (Read-Only).

- `useDerivedValues`:

  Indicates whether table values should be derived during solving. the
  ODE system. Default value is `TRUE`

- `xDimension`:

  The dimension in which the x values are defined (Read-Only).

## Methods

### Public methods

- [`TableFormula$addPoints()`](#method-TableFormula-addPoints)

- [`TableFormula$removePoint()`](#method-TableFormula-removePoint)

- [`TableFormula$clearPoints()`](#method-TableFormula-clearPoints)

- [`TableFormula$setPoints()`](#method-TableFormula-setPoints)

- [`TableFormula$print()`](#method-TableFormula-print)

- [`TableFormula$valueAt()`](#method-TableFormula-valueAt)

- [`TableFormula$printFormula()`](#method-TableFormula-printFormula)

Inherited methods

- [`rSharp::NetObject$.printClass()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printClass)
- [`rSharp::NetObject$.printLine()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printLine)
- [`rSharp::NetObject$call()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-call)
- [`rSharp::NetObject$get()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-get)
- [`rSharp::NetObject$getFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getFields)
- [`rSharp::NetObject$getMemberSignature()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMethods)
- [`rSharp::NetObject$getProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getProperties)
- [`rSharp::NetObject$getStaticFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticProperties)
- [`rSharp::NetObject$set()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-set)
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.html#method-initialize)

------------------------------------------------------------------------

### Method `addPoints()`

Adds one or more points to a table

#### Usage

    TableFormula$addPoints(xValues, yValues)

#### Arguments

- `xValues`:

  x values (single value or array) in base unit for XDimension

- `yValues`:

  y values (single value or array) in base unit for Dimension

------------------------------------------------------------------------

### Method `removePoint()`

Remove the point having the same x and y from the table

#### Usage

    TableFormula$removePoint(xValue, yValue)

#### Arguments

- `xValue`:

  xValue value in base unit for XDimension

- `yValue`:

  yValue value in base unit for Dimension

------------------------------------------------------------------------

### Method `clearPoints()`

Remove all points from the table

#### Usage

    TableFormula$clearPoints()

------------------------------------------------------------------------

### Method `setPoints()`

Replace all points defined in the table with the new values given. This
is a convenience method for calling `clearPoints` and `addPoints`

#### Usage

    TableFormula$setPoints(xValues, yValues)

#### Arguments

- `xValues`:

  x values (single value or array) in base unit for XDimension

- `yValues`:

  y values (single value or array) in base unit for Dimension

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    TableFormula$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `valueAt()`

Returns the y defined for the x value in base unit. If not exact match
is found, value will be interpolated between two existing points If the
table contains no point, 0 is returned

#### Usage

    TableFormula$valueAt(xValue)

#### Arguments

- `xValue`:

  x value for in base unit for which the yValue should be returned

------------------------------------------------------------------------

### Method `printFormula()`

Print the formula to the console

#### Usage

    TableFormula$printFormula()
