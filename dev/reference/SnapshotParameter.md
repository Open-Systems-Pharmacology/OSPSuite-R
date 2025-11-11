# SnapshotParameter

A parameter typically used in the definition of
`IndividualCharacteristics` covariates (Height, Weight etc...)

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SnapshotParameter`

## Active bindings

- `value`:

  Parameter value

- `unit`:

  Unit in which the value is defined

## Methods

### Public methods

- [`SnapshotParameter$new()`](#method-SnapshotParameter-new)

- [`SnapshotParameter$print()`](#method-SnapshotParameter-print)

- [`SnapshotParameter$printValue()`](#method-SnapshotParameter-printValue)

- [`SnapshotParameter$getPrintValue()`](#method-SnapshotParameter-getPrintValue)

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

    SnapshotParameter$new(netObject = NULL, value = NULL, unit = NULL)

#### Arguments

- `netObject`:

  Optional `NetObject`. If not defined, a new instance will be created

- `value`:

  Optional value of the parameter.

- `unit`:

  Optional unit of the value specified.

#### Returns

A new `SnapshotParameter` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SnapshotParameter$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `printValue()`

Print the parameter in one line

#### Usage

    SnapshotParameter$printValue(caption)

#### Arguments

- `caption`:

  Caption to display before the value of the parameter

------------------------------------------------------------------------

### Method `getPrintValue()`

Return a string for printing the parameter in one line

#### Usage

    SnapshotParameter$getPrintValue()

#### Returns

A string for printing the parameter in one line
