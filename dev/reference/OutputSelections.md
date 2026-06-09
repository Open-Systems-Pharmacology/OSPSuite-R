# OutputSelections

List of selected quantities selected as output for a given simulation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `OutputSelections`

## Active bindings

- `allOutputs`:

  Returns all outputs defined in the selection

## Methods

### Public methods

- [`OutputSelections$clear()`](#method-OutputSelections-clear)

- [`OutputSelections$addQuantity()`](#method-OutputSelections-addQuantity)

- [`OutputSelections$removeQuantity()`](#method-OutputSelections-removeQuantity)

- [`OutputSelections$print()`](#method-OutputSelections-print)

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
- [`DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.html#method-initialize)

------------------------------------------------------------------------

### `OutputSelections$clear()`

Removes all selected output from the selection

#### Usage

    OutputSelections$clear()

------------------------------------------------------------------------

### `OutputSelections$addQuantity()`

Adds a quantity as selected

#### Usage

    OutputSelections$addQuantity(quantity)

#### Arguments

- `quantity`:

  Quantity to add to the selection

------------------------------------------------------------------------

### `OutputSelections$removeQuantity()`

Removes a quantity from the selection

#### Usage

    OutputSelections$removeQuantity(quantity)

#### Arguments

- `quantity`:

  Quantity to remove from the selection

------------------------------------------------------------------------

### `OutputSelections$print()`

Print the object to the console

#### Usage

    OutputSelections$print(...)

#### Arguments

- `...`:

  Rest arguments.
