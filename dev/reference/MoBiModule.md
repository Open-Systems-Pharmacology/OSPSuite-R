# MoBi Module \# OR SHOULD WE CALL IT AN OspModule ?

A MoBi module, either loaded from a project or from a pkml file

## Value

A named list of `BuildingBlock` objects, with names being the names of
the PV BBs.

A named list of `BuildingBlock` objects, with names being the names of
the IC BBs.

A named list of `BuildingBlock` objects, with names being the names of
the PV BBs.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `MoBiModule`

## Active bindings

- `name`:

  Name of the module (read-only)

- `isPKSimModule`:

  Whether the module is a PK-Sim module (read-only)

- `mergeBehavior`:

  Merge behavior of the module (read/write)

- `parameterValuesBBnames`:

  Names of the Parameter Values Building Blocks (PV BBs) in the module
  (read-only)

- `initialConditionsBBnames`:

  Names of the Initial Conditions Building Blocks (IC BBs) in the module
  (read-only)

## Methods

### Public methods

- [`MoBiModule$new()`](#method-MoBiModule-new)

- [`MoBiModule$getParameterValuesBBs()`](#method-MoBiModule-getParameterValuesBBs)

- [`MoBiModule$getInitialConditionsBBs()`](#method-MoBiModule-getInitialConditionsBBs)

- [`MoBiModule$print()`](#method-MoBiModule-print)

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

    MoBiModule$new(netObject)

#### Arguments

- `netObject`:

  Reference to `NetObject` .NET MoBi-module object

#### Returns

A new `MoBiModule` object.

------------------------------------------------------------------------

### Method `getParameterValuesBBs()`

Get the list of Parameter Values Building Blocks (PV BBs) in the module.

#### Usage

    MoBiModule$getParameterValuesBBs(names = NULL, stopIfNotFound = TRUE)

#### Arguments

- `names`:

  Optional names of the Parameter Values Building Block to retrieve. If
  `NULL`, returns all PV BBs.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if any of the specified
  parameter values BB is not present in the project.

------------------------------------------------------------------------

### Method `getInitialConditionsBBs()`

Get the list of Initial Conditions Building Blocks (IC BBs) in the
module.

#### Usage

    MoBiModule$getInitialConditionsBBs(names = NULL, stopIfNotFound = TRUE)

#### Arguments

- `names`:

  Optional names of the Initial Conditions Building Block to retrieve.
  If `NULL`, returns all IC BBs.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if any of the specified
  initial conditions BB is not present in the project.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    MoBiModule$print(...)

#### Arguments

- `...`:

  Rest arguments.
