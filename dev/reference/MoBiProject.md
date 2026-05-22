# MoBi Project

A MoBi project, containing modules, building blocks, simulations, etc.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\> `MoBiProject`

## Active bindings

- `sourceFile`:

  Path to the file the project was loaded from (read-only)

- `simulationNames`:

  Names of the simulations that are present in the project (read-only)

- `moduleNames`:

  Names of the modules that are present in the project (read-only)

- `parameterIdentificationNames`:

  Names of the parameter identifications that are present in the project
  (read-only)

- `individualNames`:

  Names of the individuals that are present in the project (read-only)

- `expressionProfilesNames`:

  Names of the expression profiles that are present in the project
  (read-only)

- `defaultSimulationSettings`:

  Default simulation settings of the project (read-only)

## Methods

### Public methods

- [`MoBiProject$new()`](#method-MoBiProject-initialize)

- [`MoBiProject$getSimulation()`](#method-MoBiProject-getSimulation)

- [`MoBiProject$getObservedData()`](#method-MoBiProject-getObservedData)

- [`MoBiProject$getModules()`](#method-MoBiProject-getModules)

- [`MoBiProject$getIndividual()`](#method-MoBiProject-getIndividual)

- [`MoBiProject$getExpressionProfiles()`](#method-MoBiProject-getExpressionProfiles)

- [`MoBiProject$createSimulationConfiguration()`](#method-MoBiProject-createSimulationConfiguration)

- [`MoBiProject$print()`](#method-MoBiProject-print)

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

### `MoBiProject$new()`

Initialize a new instance of the class

Should not be directly used. Instead, use function
[`loadMoBiProject()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadMoBiProject.md)
to load a project.

#### Usage

    MoBiProject$new(netObject, sourceFile = NULL)

#### Arguments

- `netObject`:

  Underlying reference to the MoBi project in the simulation engine.

- `sourceFile`:

  (Optional) File used to load the project

#### Returns

A new `MoBiProject` object.

------------------------------------------------------------------------

### `MoBiProject$getSimulation()`

Load a simulation from the project

#### Usage

    MoBiProject$getSimulation(simulationName, stopIfNotFound = TRUE)

#### Arguments

- `simulationName`:

  Name of the simulation.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if the simulation with the
  given name is not present in the project. If `FALSE`, `NULL` is
  returned.

#### Returns

A `Simulation` object, if the simulation with the given name is present
in the project. `NULL` if no such simulation is available and
`stopIfNotFound = FALSE`.

------------------------------------------------------------------------

### `MoBiProject$getObservedData()`

Get observed data present in the project.

#### Usage

    MoBiProject$getObservedData(dataSetNames = NULL, stopIfNotFound = TRUE)

#### Arguments

- `dataSetNames`:

  Optional. List of names of observed data sets to retrieve from
  project. If `NULL`, all data sets are returned.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if any of the specified data
  sets is not present in the project. Otherwise, `NULL` is returned for
  the data sets that are not found.

#### Returns

A named list of `DataSet` objects.

------------------------------------------------------------------------

### `MoBiProject$getModules()`

Get modules present in the project.

#### Usage

    MoBiProject$getModules(names = NULL, stopIfNotFound = TRUE)

#### Arguments

- `names`:

  Optional. Names of the modules to retrieve. If `NULL`, all modules are
  returned.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if any of the specified
  modules is not present in the project. Otherwise, `NULL` is returned
  for the modules that are not found.

#### Returns

A named list of `MoBiModule` objects.

------------------------------------------------------------------------

### `MoBiProject$getIndividual()`

Get a specified individual from the project

#### Usage

    MoBiProject$getIndividual(name, stopIfNotFound = TRUE)

#### Arguments

- `name`:

  Name of the individual

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if the specified individual is
  not present in the project.

#### Returns

An object of the type `IndividualBuildingBlock`. `NULL` if the project
does not contain such an individual and `stopIfNotFound = FALSE`.

------------------------------------------------------------------------

### `MoBiProject$getExpressionProfiles()`

Get specified expression profiles from the project.

#### Usage

    MoBiProject$getExpressionProfiles(names, stopIfNotFound = TRUE)

#### Arguments

- `names`:

  List of names of the expression profiles to retrieve.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if any of the specified
  expression profiles is not present in the project.

#### Returns

A named list of objects of the type `BuildingBlock`. If
`stopIfNotFound = FALSE`, only the expression profiles that are present
in the project are returned.

------------------------------------------------------------------------

### `MoBiProject$createSimulationConfiguration()`

Create a simulation configuration.

#### Usage

    MoBiProject$createSimulationConfiguration(
      modulesNames,
      individualName = NULL,
      expressionProfilesNames = NULL,
      selectedInitialConditions = NULL,
      selectedParameterValues = NULL,
      settings = NULL
    )

#### Arguments

- `modulesNames`:

  A list of the modules from which to create the simulation. All defined
  modules must be present in the project. The order of module names
  defines the order in which the modules will be combined to a
  simulation!

- `individualName`:

  Optional, name of the individual.

- `expressionProfilesNames`:

  Optional, list of expression profiles to apply to the simulation.

- `selectedInitialConditions`:

  By default, the first Initial Conditions (IC) building block (BB) of
  each module will be selected. If a module has multiple IC BBs, it is
  possible to specify which IC BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the IC BB. By explicitly setting the value for a specific module to
  `NULL`, no IC BB from the specified module will be applied. Names that
  do not match any module in `modulesNames` raise an error.

- `selectedParameterValues`:

  By default, the first Parameter Values (PV) building block (BB) of
  each module will be selected. If a module has multiple PV BBs, it is
  possible to specify which PV BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the PV BB. By explicitly setting the value for a specific module to
  `NULL`, no PV BB from the specified module will be applied. Names that
  do not match any module in `modulesNames` raise an error.

- `settings`:

  Optional, a `SimulationSettings` object defining the simulation
  settings. If `NULL` (default), the default simulation settings of the
  project will be used.

#### Returns

A `SimulationConfiguration` object.

------------------------------------------------------------------------

### `MoBiProject$print()`

Print the object to the console

#### Usage

    MoBiProject$print(printClassProperties = FALSE, ...)

#### Arguments

- `printClassProperties`:

  Whether to print class properties.

- `...`:

  Rest arguments.
