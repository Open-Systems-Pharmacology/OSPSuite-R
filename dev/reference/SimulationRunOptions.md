# SimulationRunOptions

Options to be passed to the simulation engine

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\> `SimulationRunOptions`

## Active bindings

- `numberOfCores`:

  (Maximal) number of cores to be used. This is only relevant when
  simulating a population simulation. Default is
  `getOSPSuiteSetting("numberOfCores")`.

- `showProgress`:

  Specifies whether a progress bar should be shown during population
  simulations. If `TRUE`, a progress bar is shown in the console,
  indicating the number of already executed simulations from the total
  population size. The progress bar does not indicate the progress of a
  single simulation. This option only applies to population simulations
  and has no effect on individual simulations. Default is
  `getOSPSuiteSetting("showProgress")`

## Methods

### Public methods

- [`SimulationRunOptions$new()`](#method-SimulationRunOptions-initialize)

- [`SimulationRunOptions$print()`](#method-SimulationRunOptions-print)

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

### `SimulationRunOptions$new()`

Initialize a new instance of the class

#### Usage

    SimulationRunOptions$new(
      numberOfCores = NULL,
      checkForNegativeValues = NULL,
      showProgress = NULL
    )

#### Arguments

- `numberOfCores`:

  Number of cores to use for the simulation. Default value is
  `getOSPSuiteSetting("numberOfCores")`

- `checkForNegativeValues`:

  **\[deprecated\]** Use `sim$solver$checkForNegativeValues` instead.

- `showProgress`:

  Should a progress bar be displayed during population simulations. If
  `TRUE`, a progress bar is shown in the console, indicating the number
  of already executed simulations from the total population size. The
  progress bar does not indicate the progress of a single simulation.
  This option only applies to population simulations and has no effect
  on individual simulations. Default value is
  `getOSPSuiteSetting("showProgress")`

#### Returns

A new `SimulationRunOptions` object.

------------------------------------------------------------------------

### `SimulationRunOptions$print()`

Print the object to the console

#### Usage

    SimulationRunOptions$print(...)

#### Arguments

- `...`:

  Rest arguments.
