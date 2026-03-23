# MoBi Project

A MoBi project, containing modules, building blocks, simulations, etc.

## Value

A new `MoBiProject` object.

A `Simulation` object, if the simulation with the given name is present
in the project. `NULL` if no such simulation is available and
`stopIfNotFound = FALSE`.

A named list of `DataSet` objects.

A named list of `MoBiModule` objects.

An object of the type `BuildingBlock`. `NULL` if the project does not
contain such an individual and `stopIfNotFound = FALSE`.

A named list of objects of the type `BuildingBlock`. If
`stopIfNotFound = FALSE`, only the expression profiles that are present
in the project are returned.

A `SimulationConfiguration` object.

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
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

## Methods

### Public methods

- [`MoBiProject$new()`](#method-MoBiProject-new)

- [`MoBiProject$getSimulation()`](#method-MoBiProject-getSimulation)

- [`MoBiProject$getObservedData()`](#method-MoBiProject-getObservedData)

- [`MoBiProject$getModules()`](#method-MoBiProject-getModules)

- [`MoBiProject$getIndividual()`](#method-MoBiProject-getIndividual)

- [`MoBiProject$getExpressionProfiles()`](#method-MoBiProject-getExpressionProfiles)

- [`MoBiProject$createSimulationConfiguration()`](#method-MoBiProject-createSimulationConfiguration)

- [`MoBiProject$print()`](#method-MoBiProject-print)

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

Should not be directly used. Instead, use function
[`loadMoBiProject()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/loadMoBiProject.md)
to load a project.

#### Usage

    MoBiProject$new(netObject, sourceFile = NULL)

#### Arguments

- `netObject`:

  Reference to `NetObject` .NET MoBi-project object

- `sourceFile`:

  (Optional) File used to load the project

------------------------------------------------------------------------

### Method `getSimulation()`

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

------------------------------------------------------------------------

### Method `getObservedData()`

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

------------------------------------------------------------------------

### Method `getModules()`

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

------------------------------------------------------------------------

### Method `getIndividual()`

Get a specified individual from the project

#### Usage

    MoBiProject$getIndividual(name, stopIfNotFound = TRUE)

#### Arguments

- `name`:

  Name of the individual

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if the specified individual is
  not present in the project.

------------------------------------------------------------------------

### Method `getExpressionProfiles()`

Get specified expression profiles from the project.

#### Usage

    MoBiProject$getExpressionProfiles(names, stopIfNotFound = TRUE)

#### Arguments

- `names`:

  List of names of the expression profiles to retrieve.

- `stopIfNotFound`:

  If `TRUE` (default), an error is thrown if any of the specified
  expression profiles is not present in the project.

------------------------------------------------------------------------

### Method [`createSimulationConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createSimulationConfiguration.md)

Create a simulation configuration.

#### Usage

    MoBiProject$createSimulationConfiguration(
      modulesNames,
      individualName = NULL,
      expressionProfilesNames = NULL,
      initialConditions = NULL,
      parameterValues = NULL
    )

#### Arguments

- `modulesNames`:

  A list of the modules from which to create in simulation. All defined
  modules must be present in the project. The order of module names
  defines the order in which the modules will be combined to a
  simulation!

- `individualName`:

  Optional, name of the individual.

- `expressionProfilesNames`:

  Optional, list of expression profiles to apply to the simulation.

- `initialConditions`:

  By default, the first Initial Conditions (IC) building block (BB) of
  each module will be selected. If a module has multiple IC BBs, it is
  possible to specify which IC BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the IC BB. If `NULL`, all modules will use the first IC BB, if
  available. When providing a list, the value can also be set to `NULL`,
  which means that no IC BB from the specified module will be selected.

- `parameterValues`:

  By default, the first Parameter Values (PV) building block (BB) of
  each module will be selected. If a module has multiple PV BBs, it is
  possible to specify which PV BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the PV BB. If `NULL`, all modules will use the first PV BB, if
  available. When providing a list, the value can also be set to `NULL`,
  which means that no PV BB from the specified module will be selected.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    MoBiProject$print(...)

#### Arguments

- `...`:

  Rest arguments.
