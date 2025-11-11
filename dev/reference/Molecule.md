# Molecule

A molecule defined in a compartment of the system

## Details

Derived from
[Quantity](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.md),
please see base class documentation.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\>
[`ospsuite::Entity`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Entity.md)
-\>
[`ospsuite::Quantity`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Quantity.md)
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
- [`ospsuite::Quantity$getPrintValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/Quantity.html#method-Quantity-getPrintValue)
- [`ospsuite::Quantity$hasUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/Quantity.html#method-Quantity-hasUnit)
- [`ospsuite::Quantity$printQuantityValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/Quantity.html#method-Quantity-printQuantityValue)
- [`ospsuite::Quantity$printValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/Quantity.html#method-Quantity-printValue)
- [`ospsuite::Quantity$reset()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/Quantity.html#method-Quantity-reset)
- [`ospsuite::Quantity$setValue()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/Quantity.html#method-Quantity-setValue)

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
