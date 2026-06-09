# SimulationBatchOptions

Options to be passed to the `SimulationBatch`.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SimulationBatchOptions`

## Active bindings

- `variableParameters`:

  Vector of absolute parameter paths to be varied in a simulation batch

- `variableMolecules`:

  Vector of absolute molecule paths to be varied in a simulation batch

## Methods

### Public methods

- [`SimulationBatchOptions$new()`](#method-SimulationBatchOptions-initialize)

- [`SimulationBatchOptions$print()`](#method-SimulationBatchOptions-print)

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

### `SimulationBatchOptions$new()`

Initialize a new instance of the class

#### Usage

    SimulationBatchOptions$new(variableParameters = NULL, variableMolecules = NULL)

#### Arguments

- `variableParameters`:

  Vector of absolute parameter paths to be varied in a simulation batch

- `variableMolecules`:

  Vector of absolute molecule paths to be varied in a simulation batch

#### Returns

A new `SimulationBatchOptions` object.

------------------------------------------------------------------------

### `SimulationBatchOptions$print()`

Print the object to the console

#### Usage

    SimulationBatchOptions$print(...)

#### Arguments

- `...`:

  Rest arguments.
