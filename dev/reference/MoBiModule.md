# MoBi Module

A MoBi module, either loaded from a project or from a pkml file

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `MoBiModule`

## Active bindings

- `name`:

  Name of the module (read-only)

- `isPKSimModule`:

  Whether the module is a PK-Sim module (read-only)

- `mergeBehavior`:

  Merge behavior of the module. Must be one of "Extend" or "Overwrite".

- `parameterValuesBBnames`:

  Names of the Parameter Values Building Blocks (PV BBs) in the module
  (read-only)

- `initialConditionsBBnames`:

  Names of the Initial Conditions Building Blocks (IC BBs) in the module
  (read-only)

## Methods

### Public methods

- [`MoBiModule$new()`](#method-MoBiModule-initialize)

- [`MoBiModule$getParameterValuesBBs()`](#method-MoBiModule-getParameterValuesBBs)

- [`MoBiModule$getInitialConditionsBBs()`](#method-MoBiModule-getInitialConditionsBBs)

- [`MoBiModule$getMoleculesBB()`](#method-MoBiModule-getMoleculesBB)

- [`MoBiModule$addBuildingBlocks()`](#method-MoBiModule-addBuildingBlocks)

- [`MoBiModule$removeBuildingBlock()`](#method-MoBiModule-removeBuildingBlock)

- [`MoBiModule$print()`](#method-MoBiModule-print)

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

### `MoBiModule$new()`

Initialize a new instance of the class

#### Usage

    MoBiModule$new(netObject)

#### Arguments

- `netObject`:

  Underlying reference to the MoBi module in the simulation engine.

#### Returns

A new `MoBiModule` object.

------------------------------------------------------------------------

### `MoBiModule$getParameterValuesBBs()`

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

#### Returns

A named list of `BuildingBlock` objects, with names being the names of
the PV BBs.

------------------------------------------------------------------------

### `MoBiModule$getInitialConditionsBBs()`

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

#### Returns

A named list of `BuildingBlock` objects, with names being the names of
the IC BBs.

------------------------------------------------------------------------

### `MoBiModule$getMoleculesBB()`

Get the `Molecules` Building Block of the module, if any.

#### Usage

    MoBiModule$getMoleculesBB()

#### Returns

A `MoleculesBuildingBlock` object exposing molecule-name queries (e.g.
`allMoleculeNames()`, `allMoleculeNamesOfType()`, `moleculeTypeFor()`),
or `NULL` if the module has no Molecules BB.

------------------------------------------------------------------------

### `MoBiModule$addBuildingBlocks()`

Add one or more building blocks to the module.

Single-type building blocks (Molecules, Reactions, Spatial Structure,
Passive Transports, Observers, Event Groups) can appear at most once per
module; trying to add a second BB of the same single type raises an
error. Initial Conditions and Parameter Values BBs may appear multiple
times.

#### Usage

    MoBiModule$addBuildingBlocks(buildingBlocks)

#### Arguments

- `buildingBlocks`:

  A `BuildingBlock`, a list of `BuildingBlock` objects, or `NULL` /
  empty list (no-op).

#### Returns

The module, invisibly.

------------------------------------------------------------------------

### `MoBiModule$removeBuildingBlock()`

Remove a building block from the module by its name and type.

Single-type building blocks (Molecules, Reactions, Spatial Structure,
Passive Transports, Observers, Event Groups) are matched on type and the
lone BB's `Name` is verified against `name`. Multi-type building blocks
(Initial Conditions, Parameter Values) are looked up by name within
their type. Expression Profile and Individual are not module-level and
raise an error.

#### Usage

    MoBiModule$removeBuildingBlock(name, type)

#### Arguments

- `name`:

  Name of the building block to remove (the BB's `Name` property).

- `type`:

  Type of the building block to remove. One of the values defined in
  `BuildingBlockTypes` (excluding `Expression Profile` and
  `Individual`).

#### Returns

The module, invisibly.

------------------------------------------------------------------------

### `MoBiModule$print()`

Print the object to the console

#### Usage

    MoBiModule$print(printClassProperties = FALSE, ...)

#### Arguments

- `printClassProperties`:

  Logical, whether to print class properties (default: `FALSE`). If
  `TRUE`, calls first the `print` method of the parent class. Useful for
  debugging.

- `...`:

  Rest arguments.
