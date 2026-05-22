# IndividualBuildingBlock

An `Individual` building block. Subclass of
[BuildingBlock](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.md)
that exposes the demographic properties of the underlying individual as
read-only fields (species, population, gender, age, gestational age,
height, weight).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\>
[`BuildingBlock`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.md)
-\> `IndividualBuildingBlock`

## Active bindings

- `species`:

  Species of the individual. Read-only.

- `population`:

  Population of the individual. May be `NULL` for non-human species.
  Read-only.

- `gender`:

  Gender of the individual. Read-only.

- `age`:

  Age of the individual. Read-only.

- `gestationalAge`:

  Gestational age of the individual. Read-only.

- `height`:

  Height of the individual. Read-only.

- `weight`:

  Weight of the individual. Read-only.

## Methods

### Public methods

- [`IndividualBuildingBlock$new()`](#method-IndividualBuildingBlock-initialize)

- [`IndividualBuildingBlock$print()`](#method-IndividualBuildingBlock-print)

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

### `IndividualBuildingBlock$new()`

Initialize a new instance of the class. Origin-data values are read once
from the underlying object and cached on the R6 instance; subsequent
field access does not call back into the simulation engine.

#### Usage

    IndividualBuildingBlock$new(netObject)

#### Arguments

- `netObject`:

  Reference to the underlying `Individual` building block.

#### Returns

A new `IndividualBuildingBlock` object.

------------------------------------------------------------------------

### `IndividualBuildingBlock$print()`

Print the object to the console.

#### Usage

    IndividualBuildingBlock$print(...)

#### Arguments

- `...`:

  Rest arguments.
