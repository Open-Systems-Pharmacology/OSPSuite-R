# Parameter

A model parameter

## Details

Derived from
[Quantity](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.md),
please see base class documentation.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\>
[`Entity`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Entity.md)
-\>
[`Quantity`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.md)
-\> `Parameter`

## Active bindings

- `isStateVariable`:

  Returns `TRUE` is the parameter has a RHS otherwise `FALSE`. Setting
  the value to `FALSE` will delete the RHS Formula. Setting it to `TRUE`
  is not currently supported and will throw an error.

- `rhsFormula`:

  An instance of a `Formula` object representing the RHS Formula
  (Read-Only)

## Methods

### Public methods

- [`Parameter$new()`](#method-Parameter-initialize)

- [`Parameter$print()`](#method-Parameter-print)

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
- [`Quantity$getPrintValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.html#method-getPrintValue)
- [`Quantity$hasUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.html#method-hasUnit)
- [`Quantity$printQuantityValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.html#method-printQuantityValue)
- [`Quantity$printValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.html#method-printValue)
- [`Quantity$reset()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.html#method-reset)
- [`Quantity$setValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.html#method-setValue)

------------------------------------------------------------------------

### `Parameter$new()`

Initialize a new instance of the class

#### Usage

    Parameter$new(netObject)

#### Arguments

- `netObject`:

  An
  [`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
  object.

#### Returns

A new `Parameter` object.

------------------------------------------------------------------------

### `Parameter$print()`

Print the object to the console

#### Usage

    Parameter$print(...)

#### Arguments

- `...`:

  Rest arguments.
