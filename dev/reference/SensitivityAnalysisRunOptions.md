# SensitivityAnalysisRunOptions

Options to be passed to the sensitivity analysis engine

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SensitivityAnalysisRunOptions`

## Active bindings

- `numberOfCores`:

  (Maximal) number of cores to be used. Per default set to
  `getOSPSuiteSetting("numberOfCores")`.

- `showProgress`:

  Specifies whether a progress bar should be shown during sensitivity
  analysis. If `TRUE`, a progress bar is shown in the console,
  indicating the progress of the sensitivity analysis calculations.
  Default is `getOSPSuiteSetting("showProgress")`.

## Methods

### Public methods

- [`SensitivityAnalysisRunOptions$new()`](#method-SensitivityAnalysisRunOptions-initialize)

- [`SensitivityAnalysisRunOptions$print()`](#method-SensitivityAnalysisRunOptions-print)

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

### `SensitivityAnalysisRunOptions$new()`

Initialize a new instance of the class

#### Usage

    SensitivityAnalysisRunOptions$new(numberOfCores = NULL, showProgress = NULL)

#### Arguments

- `numberOfCores`:

  Number of cores to use for the simulation. Default value is
  `getOSPSuiteSetting("numberOfCores")`

- `showProgress`:

  Should a progress bar be displayed during sensitivity analysis. If
  `TRUE`, a progress bar is shown in the console, indicating the
  progress of the sensitivity analysis calculations. Default value is
  `getOSPSuiteSetting("showProgress")`

#### Returns

A new `SensitivityAnalysisRunOptions` object.

------------------------------------------------------------------------

### `SensitivityAnalysisRunOptions$print()`

Print the object to the console

#### Usage

    SensitivityAnalysisRunOptions$print(...)

#### Arguments

- `...`:

  Rest arguments.
