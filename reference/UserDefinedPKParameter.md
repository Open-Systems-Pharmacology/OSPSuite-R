# UserDefinedPKParameter

Definition of a user defined PKParameter that can be calculated on top
of the standard PK Parameters

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\>
[`ospsuite::PKParameter`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/PKParameter.md)
-\> `UserDefinedPKParameter`

## Active bindings

- `startTime`:

  Start time in minutes for the calculation of the PK-Parameter. If not
  specified, the time will start at the first time point of the
  simulation (optional)

- `startTimeOffset`:

  Offset in minutes to apply to the start time or to the start time of
  the application identified by `startApplicationIndex`. (0 by default).

- `endTime`:

  End time in minutes for the calculation of the PK-Parameter. If not
  specified, the time will end at the last time point of the simulation
  (optional)

- `endTimeOffset`:

  Offset in minutes to apply to the end time or to the start time of the
  application identified by `endApplicationIndex`. (0 by default).

- `startApplicationIndex`:

  1-based Index of the application to use to determine the start time
  for the calculation of the PK-Parameter. If not specified, the time
  will start at the first time point of the simulation (optional)

- `endApplicationIndex`:

  1-based Index of the application to use to determine the end time for
  the calculation of the PK-Parameter. If not specified, the time will
  end at the last time point of the simulation (optional)

- `normalizationFactor`:

  Factor to use to normalized the calculated PK-Parameter. (typically
  DrugMass, Dose, DosePerBodyWeight). It is the responsibility of the
  caller to ensure that the value is in the correct unit. (optional)

- `concentrationThreshold`:

  Used in conjunction with the `threshold` parameter type. If defined,
  the time at which this concentration was reached will be calculated

- `standardPKParameter`:

  Based parameter to use to perform the PK-Analysis calculation. See
  `StandardPKParameter` enum for all possible pk parameters

## Methods

### Public methods

- [`UserDefinedPKParameter$print()`](#method-UserDefinedPKParameter-print)

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
- [`ospsuite::DotNetWrapper$initialize()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.html#method-initialize)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    UserDefinedPKParameter$print(...)

#### Arguments

- `...`:

  Rest arguments.
