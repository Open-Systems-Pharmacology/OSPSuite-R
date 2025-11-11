# SimulationPKAnalyses

pK-Analyses of a simulation (either individual or population
simulation).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
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

- [`SimulationPKAnalyses$new()`](#method-SimulationPKAnalyses-new)

- [`SimulationPKAnalyses$allPKParametersFor()`](#method-SimulationPKAnalyses-allPKParametersFor)

- [`SimulationPKAnalyses$pKParameterFor()`](#method-SimulationPKAnalyses-pKParameterFor)

- [`SimulationPKAnalyses$print()`](#method-SimulationPKAnalyses-print)

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

    SimulationPKAnalyses$new(netObject, simulation)

#### Arguments

- `netObject`:

  A `NetObject`

- `simulation`:

  Simulation for which the pkParameters were calculated

#### Returns

A new `SimulationPKAnalyses` object.

------------------------------------------------------------------------

### Method `allPKParametersFor()`

Returns all QuantityPKParameter defined for a given path

#### Usage

    SimulationPKAnalyses$allPKParametersFor(quantityPath)

#### Arguments

- `quantityPath`:

  Path for which pkParameters should be retrieved

------------------------------------------------------------------------

### Method `pKParameterFor()`

The pK Parameter defined for the given path and name

#### Usage

    SimulationPKAnalyses$pKParameterFor(quantityPath, pkParameter)

#### Arguments

- `quantityPath`:

  Path for which the pkParameter named `pkParameter` should be retrieved

- `pkParameter`:

  Name of the pkParameter to retrieve

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SimulationPKAnalyses$print(...)

#### Arguments

- `...`:

  Rest arguments.
