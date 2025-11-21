# SimulationResults

Results of a simulation run (either individual or population simulation)

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `SimulationResults`

## Active bindings

- `count`:

  the number of individual results (`Count==1` generally means that we
  are dealing with an individual simulation results).

- `simulation`:

  Reference to the `Simulation` used to calculate or import the results
  (Read-Only).

- `timeValues`:

  Vector of simulated time output values

- `allQuantityPaths`:

  List of all paths for which results are defined.

- `allIndividualIds`:

  List of Ids of all individuals that have been simulated

## Methods

### Public methods

- [`SimulationResults$new()`](#method-SimulationResults-new)

- [`SimulationResults$hasResultsForIndividual()`](#method-SimulationResults-hasResultsForIndividual)

- [`SimulationResults$getValuesByPath()`](#method-SimulationResults-getValuesByPath)

- [`SimulationResults$resultsForIndividual()`](#method-SimulationResults-resultsForIndividual)

- [`SimulationResults$print()`](#method-SimulationResults-print)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-set)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    SimulationResults$new(netObject, simulation)

#### Arguments

- `netObject`:

  An
  [`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
  object.

- `simulation`:

  Reference to the simulation object used to calculated the results

#### Returns

A new `SimulationResults` object.

------------------------------------------------------------------------

### Method `hasResultsForIndividual()`

Returns `TRUE` if results are available for the individual with id
`individualId` otherwise `FALSE`

#### Usage

    SimulationResults$hasResultsForIndividual(individualId)

#### Arguments

- `individualId`:

  Id of the individual

------------------------------------------------------------------------

### Method `getValuesByPath()`

Returns `TRUE` if results are available for the individual with id
`individualId` otherwise `FALSE`

#### Usage

    SimulationResults$getValuesByPath(path, individualIds, stopIfNotFound = TRUE)

#### Arguments

- `path`:

  Path for which values should be retrieved

- `individualIds`:

  One or more individual ids for which values should be returned

- `stopIfNotFound`:

  If `TRUE` (default) an error is thrown if no values could be found for
  the `path`/ If `FALSE`, a list of `NA` values is returned

------------------------------------------------------------------------

### Method `resultsForIndividual()`

Returns all available results for the individual with id `individualId`

#### Usage

    SimulationResults$resultsForIndividual(individualId)

#### Arguments

- `individualId`:

  Id for which the results should be returned

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SimulationResults$print(...)

#### Arguments

- `...`:

  Rest arguments.
