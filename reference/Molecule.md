# Molecule

A molecule defined in a compartment of the system

## Details

Derived from
[Quantity](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.md),
please see base class documentation.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/ObjectBase.md)
-\>
[`ospsuite::Entity`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Entity.md)
-\>
[`ospsuite::Quantity`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.md)
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

- [`Molecule$new()`](#method-Molecule-new)

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
- [`ospsuite::Quantity$getPrintValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.html#method-getPrintValue)
- [`ospsuite::Quantity$hasUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.html#method-hasUnit)
- [`ospsuite::Quantity$printQuantityValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.html#method-printQuantityValue)
- [`ospsuite::Quantity$printValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.html#method-printValue)
- [`ospsuite::Quantity$reset()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.html#method-reset)
- [`ospsuite::Quantity$setValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/Quantity.html#method-setValue)

------------------------------------------------------------------------

### Method `new()`

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

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Molecule$print(...)

#### Arguments

- `...`:

  Rest arguments.
