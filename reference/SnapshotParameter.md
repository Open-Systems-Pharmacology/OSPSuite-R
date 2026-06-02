# SnapshotParameter

A parameter typically used in the definition of
`IndividualCharacteristics` covariates (Height, Weight etc...)

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
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
