# SensitivityAnalysis

Supports Sensitivity Analysis workflow to assess the impact of input
parameters on the simulation outputs

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SensitivityAnalysis`

## Active bindings

- `simulation`:

  Reference to the `Simulation` used to calculate or import the
  sensitivity analysis results (Read-Only).

- `numberOfSteps`:

  Number of steps used for the variation of each parameter (optional,
  default specified in `ospsuiteEnv$sensitivityAnalysisConfig`)

- `variationRange`:

  Variation applied to the parameter (optional, default specified in
  `ospsuiteEnv$sensitivityAnalysisConfig`)

- `parameterPaths`:

  List of parameters to use for sensitivity calculation.If empty, the
  sensitivity will be performed automatically on all constant parameters
  that are really in use in the simulation. Constant parameter means all
  parameters with a constant value or a formula parameter with a value
  that was overridden by the user

## Methods

### Public methods

- [`SensitivityAnalysis$new()`](#method-SensitivityAnalysis-new)

- [`SensitivityAnalysis$addParameterPaths()`](#method-SensitivityAnalysis-addParameterPaths)

- [`SensitivityAnalysis$clearParameterPaths()`](#method-SensitivityAnalysis-clearParameterPaths)

- [`SensitivityAnalysis$print()`](#method-SensitivityAnalysis-print)

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

    SensitivityAnalysis$new(
      simulation,
      parameterPaths = NULL,
      numberOfSteps = ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps,
      variationRange = ospsuiteEnv$sensitivityAnalysisConfig$variationRange
    )

#### Arguments

- `simulation`:

  Simulation for which a sensitivity analysis should be performed

- `parameterPaths`:

  Vector of parameter paths to use for sensitivity calculation
  (optional).If undefined, the sensitivity will be performed
  automatically on all constant parameters of the simulation. Constant
  parameter means all parameters with a constant value or a formula
  parameter with a value that was overridden by the user

- `numberOfSteps`:

  Number of steps used for the variation of each parameter (optional,
  default specified in
  `getOSPSuiteSetting("sensitivityAnalysisConfig")`)

- `variationRange`:

  Variation applied to the parameter (optional, default specified in
  `getOSPSuiteSetting("sensitivityAnalysisConfig")`)

#### Returns

A new `SensitivityAnalysis` object.

------------------------------------------------------------------------

### Method `addParameterPaths()`

Adds the parameterPaths to the list of parameter path to vary in the
sensitivity analysis

#### Usage

    SensitivityAnalysis$addParameterPaths(parameterPaths)

#### Arguments

- `parameterPaths`:

  Parameter paths to add (single or multiple values) If no parameters
  were specified during creating of a `SensitivityAnalysis` (all
  constant parameters are considered), calling `addParameterPaths` will
  make only the manually added parameters being varied.

------------------------------------------------------------------------

### Method `clearParameterPaths()`

Removes all parameter paths defined in the Sensitivity Analysis

#### Usage

    SensitivityAnalysis$clearParameterPaths()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SensitivityAnalysis$print(...)

#### Arguments

- `...`:

  Rest arguments.
