# SimulationBatch

An optimized simulation with faster loading. The corresponding .NET
class is "OSPSuite.R.Services.ConcurrentRunSimulationBatch"

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SimulationBatch`

## Active bindings

- `simulation`:

  Underlying simulation used for the batch run. Read only.

- `runValuesIds`:

  Ids of the run values that will be executed on next run

- `id`:

  The id of the .NET wrapped object. (read-only)

## Methods

### Public methods

- [`SimulationBatch$new()`](#method-SimulationBatch-initialize)

- [`SimulationBatch$addRunValues()`](#method-SimulationBatch-addRunValues)

- [`SimulationBatch$getVariableParameters()`](#method-SimulationBatch-getVariableParameters)

- [`SimulationBatch$getVariableMolecules()`](#method-SimulationBatch-getVariableMolecules)

- [`SimulationBatch$print()`](#method-SimulationBatch-print)

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

### `SimulationBatch$new()`

Initialize a new instance of the class

#### Usage

    SimulationBatch$new(netObject, simulation)

#### Arguments

- `netObject`:

  An
  [`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
  object.

- `simulation`:

  Simulation used in the batch run

#### Returns

A new `SimulationBatch` object.

------------------------------------------------------------------------

### `SimulationBatch$addRunValues()`

Add a set of parameter and start values for next execution.

#### Usage

    SimulationBatch$addRunValues(parameterValues = NULL, initialValues = NULL)

#### Arguments

- `parameterValues`:

  Vector of parameter values to set in the simulation (default is
  `NULL`)

- `initialValues`:

  Vector of initial values to set in the simulation (default is `NULL`)

#### Details

Intended for the use with `runSimulationBatches`. The simulation batch
is executed with the sets of parameter and initial values that have been
scheduled. The set of run values is cleared after successful run.

#### Returns

Id of the values set that can be used to get the correct result from
`runSimulationBatches`.

#### Examples

    sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
    sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
    parameters <- c("Organism|Liver|Volume", "R1|k1")
    molecules <- "Organism|Liver|A"
    # Create two simulation batches.
    simulationBatch1 <- createSimulationBatch(simulation = sim1,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules)
    simulationBatch2 <- createSimulationBatch(simulation = sim2,
    parametersOrPaths = parameters,
    moleculesOrPaths = molecules)
    #Ids of run values
    ids <- c()
    ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
    ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
    ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
    ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
    res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))

------------------------------------------------------------------------

### `SimulationBatch$getVariableParameters()`

Returns a list of parameter paths that are variable in this batch.

#### Usage

    SimulationBatch$getVariableParameters()

#### Details

The order of parameters is the same as the order of parameter values
added with `$addRunValues()` method.

#### Returns

List of parameter paths, or `NULL` if no parameter is variable.

------------------------------------------------------------------------

### `SimulationBatch$getVariableMolecules()`

Returns a list of molecules paths that are variable in this batch

#### Usage

    SimulationBatch$getVariableMolecules()

#### Details

The order of molecules is the same as the order of molecule start values
added with `$addRunValues()` method.

#### Returns

List of parameter paths, or `NULL` if no molecule is variable.

------------------------------------------------------------------------

### `SimulationBatch$print()`

Print the object to the console

#### Usage

    SimulationBatch$print(...)

#### Arguments

- `...`:

  Additional arguments.

## Examples

``` r

## ------------------------------------------------
## Method `SimulationBatch$addRunValues()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
sim1 <- loadSimulation("sim1", loadFromCache = TRUE)
sim2 <- loadSimulation("sim2", loadFromCache = TRUE)
parameters <- c("Organism|Liver|Volume", "R1|k1")
molecules <- "Organism|Liver|A"
# Create two simulation batches.
simulationBatch1 <- createSimulationBatch(simulation = sim1,
parametersOrPaths = parameters,
moleculesOrPaths = molecules)
simulationBatch2 <- createSimulationBatch(simulation = sim2,
parametersOrPaths = parameters,
moleculesOrPaths = molecules)
#Ids of run values
ids <- c()
ids[[1]] <- simulationBatch1$addRunValues(parameterValues = c(1, 2), initialValues = 1)
ids[[2]] <- simulationBatch1$addRunValues(parameterValues = c(1.6, 2.4), initialValues = 3)
ids[[3]] <- simulationBatch2$addRunValues(parameterValues = c(4, 2), initialValues = 4)
ids[[4]] <- simulationBatch2$addRunValues(parameterValues = c(2.6, 4.4), initialValues = 5)
res <- runSimulationBatches(simulationBatches = list(simulationBatch1, simulationBatch2))
} # }
```
