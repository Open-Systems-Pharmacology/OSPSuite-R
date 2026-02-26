# Quantity

A quantity of the model (with unit, value) such as a Parameter or an
Amount

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\>
[`ospsuite::Entity`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/Entity.md)
-\> `Quantity`

## Active bindings

- `value`:

  The value of the quantity in unit

- `unit`:

  The base unit in which the quantity value is defined (Read-Only)

- `displayUnit`:

  The unit in which the quantity value is usually displayed (Read-Only)

- `dimension`:

  The dimension in which the quantity is defined (Read-Only)

- `allUnits`:

  the list of all supported units (Read-Only)

- `quantityType`:

  The type of the quantity (Read-Only)

- `formula`:

  An instance of a `Formula` object used by this quantity (Read-Only)

- `isTable`:

  Returns `TRUE` if the formula used by this quantity is a table formula
  otherwise `FALSE`

- `isConstant`:

  Returns `TRUE` if the formula used by this quantity is a constant
  formula otherwise `FALSE`

- `isFormula`:

  Returns `TRUE` if the formula used by this quantity is an explicit
  formula (e.g an equation) otherwise `FALSE`

- `isDistributed`:

  Returns `TRUE` if the quantity represents a quantity with an
  underlying distribution otherwise `FALSE`

- `formulaString`:

  Returns the equation of the formula for a quantity using an explicit
  formula (e.g. `isFormula == TRUE`) or `NULL` for a quantity that does
  not use an explicit formula.

- `isFixedValue`:

  Returns `TRUE` if the formula was overridden by a constant value,
  otherwise `FALSE`

- `valueOrigin`:

  The value origin of the quantity (Read-Only)

## Methods

### Public methods

- [`Quantity$new()`](#method-Quantity-new)

- [`Quantity$print()`](#method-Quantity-print)

- [`Quantity$printValue()`](#method-Quantity-printValue)

- [`Quantity$printQuantityValue()`](#method-Quantity-printQuantityValue)

- [`Quantity$getPrintValue()`](#method-Quantity-getPrintValue)

- [`Quantity$setValue()`](#method-Quantity-setValue)

- [`Quantity$hasUnit()`](#method-Quantity-hasUnit)

- [`Quantity$reset()`](#method-Quantity-reset)

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

    Quantity$new(netObject)

#### Arguments

- `netObject`:

  A `NetObject` object with the pointer to the .NET `Quantity`

#### Returns

A new `Quantity` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    Quantity$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `printValue()`

Print the name of the quantity and its value

#### Usage

    Quantity$printValue()

------------------------------------------------------------------------

### Method `printQuantityValue()`

Print the value (in scientific notation with 2 digits when needed) and
unit of the quantity

#### Usage

    Quantity$printQuantityValue(caption)

#### Arguments

- `caption`:

  Text to prepend to the value

------------------------------------------------------------------------

### Method `getPrintValue()`

Return a string for printing the value (in scientific notation with 2
digits when needed) and unit of the quantity

#### Usage

    Quantity$getPrintValue()

#### Arguments

- `caption`:

  Text to prepend to the value

#### Returns

A string for printing the quantity in one line

------------------------------------------------------------------------

### Method `setValue()`

Convert value from unit to the base unit and sets the value in base
unit.

#### Usage

    Quantity$setValue(value, unit = NULL)

#### Arguments

- `value`:

  Value to set. If unit is null, we assume that the value is in base
  unit

- `unit`:

  Optional unit in which the value is given.

------------------------------------------------------------------------

### Method [`hasUnit()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/hasUnit.md)

Returns `TRUE` if the quantity supports the given unit otherwise
`FALSE`. For the list of supported units, use `allUnits`

#### Usage

    Quantity$hasUnit(unit)

#### Arguments

- `unit`:

  Unit to check

------------------------------------------------------------------------

### Method `reset()`

Ensures that the quantity uses the value computed by its formula. It is
a shortcut for `self$isFixedValue <- false`.

#### Usage

    Quantity$reset()
