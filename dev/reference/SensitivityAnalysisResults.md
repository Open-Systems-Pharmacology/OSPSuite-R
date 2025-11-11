# SensitivityAnalysisResults

Results of a sensitivity analysis run (either individual or population
simulation).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SensitivityAnalysisResults`

## Active bindings

- `simulation`:

  Reference to the `Simulation` used to calculate or import the
  sensitivity analysis results (Read-Only).

- `count`:

  the number of pk parameter sensitivity entries

- `allPKParameterNames`:

  Returns the name of all PK-Parameters available in this results. This
  will be a subset of all potential PK-Parameters available in the
  system.

- `allQuantityPaths`:

  Returns the path of all outputs available in this results.

## Methods

### Public methods

- [`SensitivityAnalysisResults$new()`](#method-SensitivityAnalysisResults-new)

- [`SensitivityAnalysisResults$allPKParameterSensitivitiesFor()`](#method-SensitivityAnalysisResults-allPKParameterSensitivitiesFor)

- [`SensitivityAnalysisResults$pkParameterSensitivityValueFor()`](#method-SensitivityAnalysisResults-pkParameterSensitivityValueFor)

- [`SensitivityAnalysisResults$print()`](#method-SensitivityAnalysisResults-print)

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

    SensitivityAnalysisResults$new(netObject, simulation)

#### Arguments

- `netObject`:

  A `NetObject`.

- `simulation`:

  Reference to the simulation object used to calculated the results.

#### Returns

A new `SensitivityAnalysisResults` object.

------------------------------------------------------------------------

### Method `allPKParameterSensitivitiesFor()`

Returns the `PKParameterSensitivity` for a given `pkParameter` and
output participating to a total sensitivity greater or equal to
`totalSensitivityThreshold`.

#### Usage

    SensitivityAnalysisResults$allPKParameterSensitivitiesFor(
      pkParameterName,
      outputPath,
      totalSensitivityThreshold =
        ospsuiteEnv$sensitivityAnalysisConfig$totalSensitivityThreshold
    )

#### Arguments

- `pkParameterName`:

  Name of `pkParameter` for which sensitivity should be retrieved.

- `outputPath`:

  Path of the output for which the sensitivity should be retrieved

- `totalSensitivityThreshold`:

  Threshold used to filter out the most sensitive parameter. A threshold
  of `0.9` means that only parameter participating to a total of `90`
  percent of the sensitivity would be returned. A value of `1` would
  return the sensitivity for all parameters.

------------------------------------------------------------------------

### Method `pkParameterSensitivityValueFor()`

Returns the sensitivity value for a given `pkParameter`, output and
model parameter (either by path or by name). If the sensitivity result
does not exist, returns `NaN`.

#### Usage

    SensitivityAnalysisResults$pkParameterSensitivityValueFor(
      pkParameterName,
      outputPath,
      parameterName = NULL,
      parameterPath = NULL
    )

#### Arguments

- `pkParameterName`:

  Name of `pkParameter` for which sensitivity should be retrieved.

- `outputPath`:

  Path of the output for which the sensitivity should be retrieved.

- `parameterName`:

  Name of the sensitivity parameter for which the sensitivity should be
  retrieved.

- `parameterPath`:

  Path of the sensitivity parameter for which the sensitivity should be
  retrieved. Wildcards (\*) not accepted.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SensitivityAnalysisResults$print(...)

#### Arguments

- `...`:

  Rest arguments.
