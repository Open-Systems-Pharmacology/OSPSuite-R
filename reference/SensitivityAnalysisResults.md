# SensitivityAnalysisResults

Results of a sensitivity analysis run (either individual or population
simulation).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
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
  return the sensitivity for all parameters. For a detailed explanation
  of how this threshold is used, see [OSPS
  documentation](https://docs.open-systems-pharmacology.org/shared-tools-and-example-workflows/sensitivity-analysis).
  The default value can be retrieved with
  `getOSPSuiteSetting("sensitivityAnalysisConfig")$totalSensitivityThreshold`
  and can be changed by setting
  `ospsuiteEnv$sensitivityAnalysisConfig$totalSensitivityThreshold <- newValue`.

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
