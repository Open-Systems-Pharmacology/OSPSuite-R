# ParameterRange

A parameter range typically used in the definition of
`PopulationCharacteristics` covariates (Height, Weight etc...)

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `ParameterRange`

## Active bindings

- `min`:

  Minimum value for the parameter range

- `max`:

  Maximum value for the parameter range

- `unit`:

  Unit in which the value is defined

## Methods

### Public methods

- [`ParameterRange$new()`](#method-ParameterRange-new)

- [`ParameterRange$print()`](#method-ParameterRange-print)

- [`ParameterRange$printValue()`](#method-ParameterRange-printValue)

- [`ParameterRange$getPrintValue()`](#method-ParameterRange-getPrintValue)

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

    ParameterRange$new(netObject = NULL, min = NULL, max = NULL, unit = NULL)

#### Arguments

- `netObject`:

  Optional `NetObject` of `ParameterRange`. If not defined, a new
  instance will be created

- `min`:

  Optional minimum value for the range

- `max`:

  Optional minimum value for the range

- `unit`:

  Optional unit of the specified min and max

#### Returns

A new `ParameterRange` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    ParameterRange$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `printValue()`

Print the parameter in one line

#### Usage

    ParameterRange$printValue(caption)

#### Arguments

- `caption`:

  Caption to display before the value of the parameter

------------------------------------------------------------------------

### Method `getPrintValue()`

Return a string for printing the parameter in one line

#### Usage

    ParameterRange$getPrintValue()

#### Returns

A string for printing the parameter in one line
