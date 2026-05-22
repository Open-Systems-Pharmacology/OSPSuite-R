# Building block

A representation of a building block

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `BuildingBlock`

## Active bindings

- `type`:

  Type of the building block (Spatial structure, molecules, reactions,
  etc)

- `id`:

  ID of the building block

- `name`:

  Name of the building block. Read-only.

## Methods

### Public methods

- [`BuildingBlock$new()`](#method-BuildingBlock-initialize)

- [`BuildingBlock$print()`](#method-BuildingBlock-print)

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

### `BuildingBlock$new()`

Initialize a new instance of the class

#### Usage

    BuildingBlock$new(netObject, type)

#### Arguments

- `netObject`:

  Underlying reference to the building block in the simulation engine.

- `type`:

  Type of the building block. Must be one of the `BuildingBlockTypes`.

#### Returns

A new `BuildingBlock` object.

------------------------------------------------------------------------

### `BuildingBlock$print()`

Print the object to the console

#### Usage

    BuildingBlock$print(printClassProperties = FALSE, ...)

#### Arguments

- `printClassProperties`:

  Logical, whether to print class properties (default: `FALSE`). If
  `TRUE`, calls first the `print` method of the parent class. Useful for
  debugging.

- `...`:

  Rest arguments.
