# SimulationPKAnalyses

pK-Analyses of a simulation (either individual or population
simulation).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SimulationPKAnalyses`

## Active bindings

- `simulation`:

  Reference to the `Simulation` used to calculate or import the
  PK-Analyses (Read-Only)

- `allPKParameterNames`:

  Returns the name of all pk parameters for which a value is available

- `allQuantityPaths`:

  Returns the path of all quantities for which pk parameters were
  calculated

## Methods

### Public methods

- [`SimulationPKAnalyses$new()`](#method-SimulationPKAnalyses-initialize)

- [`SimulationPKAnalyses$allPKParametersFor()`](#method-SimulationPKAnalyses-allPKParametersFor)

- [`SimulationPKAnalyses$pKParameterFor()`](#method-SimulationPKAnalyses-pKParameterFor)

- [`SimulationPKAnalyses$print()`](#method-SimulationPKAnalyses-print)

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

### `SimulationPKAnalyses$new()`

Initialize a new instance of the class

#### Usage

    SimulationPKAnalyses$new(netObject, simulation)

#### Arguments

- `netObject`:

  A `NetObject`

- `simulation`:

  Simulation for which the pkParameters were calculated

#### Returns

A new `SimulationPKAnalyses` object.

------------------------------------------------------------------------

### `SimulationPKAnalyses$allPKParametersFor()`

Returns all QuantityPKParameter defined for a given path

#### Usage

    SimulationPKAnalyses$allPKParametersFor(quantityPath)

#### Arguments

- `quantityPath`:

  Path for which pkParameters should be retrieved

------------------------------------------------------------------------

### `SimulationPKAnalyses$pKParameterFor()`

The pK Parameter defined for the given path and name

#### Usage

    SimulationPKAnalyses$pKParameterFor(quantityPath, pkParameter)

#### Arguments

- `quantityPath`:

  Path for which the pkParameter named `pkParameter` should be retrieved

- `pkParameter`:

  Name of the pkParameter to retrieve

------------------------------------------------------------------------

### `SimulationPKAnalyses$print()`

Print the object to the console

#### Usage

    SimulationPKAnalyses$print(...)

#### Arguments

- `...`:

  Rest arguments.
