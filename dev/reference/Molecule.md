# Molecule

A molecule defined in a compartment of the system

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
-\> `Molecule`

## Active bindings

- `value`:

  Start value of the molecule

- `scaleDivisor`:

  Scale divisor. Its purpose is to reduce numerical noise and to enhance
  computation performance. see
  <https://docs.open-systems-pharmacology.org/working-with-mobi/mobi-documentation/model-building-components#import-molecule-and-parameter-start-values-from-excel>

## Methods

### Public methods

- [`Molecule$new()`](#method-Molecule-initialize)

- [`Molecule$print()`](#method-Molecule-print)

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

### `Molecule$new()`

Initialize a new instance of the class

#### Usage

    Molecule$new(netObject)

#### Arguments

- `netObject`:

  An
  [`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
  object.

#### Returns

A new `Molecule` object.

------------------------------------------------------------------------

### `Molecule$print()`

Print the object to the console

#### Usage

    Molecule$print(...)

#### Arguments

- `...`:

  Rest arguments.
