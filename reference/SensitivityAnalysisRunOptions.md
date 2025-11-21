# SensitivityAnalysisRunOptions

Options to be passed to the sensitivity analysis engine

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `SensitivityAnalysisRunOptions`

## Active bindings

- `numberOfCores`:

  (Maximal) number of cores to be used. Per default set to
  `getOSPSuiteSetting("numberOfCores")`.

- `showProgress`:

  Specifies whether progress bar should be shown during sensitivity
  analysis run. Default is `getOSPSuiteSetting("showProgress")`.

## Methods

### Public methods

- [`SensitivityAnalysisRunOptions$new()`](#method-SensitivityAnalysisRunOptions-new)

- [`SensitivityAnalysisRunOptions$print()`](#method-SensitivityAnalysisRunOptions-print)

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

    SensitivityAnalysisRunOptions$new(numberOfCores = NULL, showProgress = NULL)

#### Arguments

- `numberOfCores`:

  Number of cores to use for the simulation. Default value is
  `getOSPSuiteSetting("numberOfCores")`

- `showProgress`:

  Should a progress information be displayed. Default value is
  `getOSPSuiteSetting("showProgress")`

#### Returns

A new `SensitivityAnalysisRunOptions` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SensitivityAnalysisRunOptions$print(...)

#### Arguments

- `...`:

  Rest arguments.
