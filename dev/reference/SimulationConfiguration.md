# Simulation configuration

Configuration of a simulation. Contains description of the modules used
for the simulation, selected Parameter Values (PV) and Initial
Conditions (IC), and molecule calculation methods.

## Value

A new `SimulationConfiguration` object.

## Active bindings

- `individual`:

  A building block of type "Individual" used in the configuration. Can
  be `NULL` if no Individual should be applied.

- `expressionProfiles`:

  A list of building blocks of type "Expression Profile" used in the
  configuration. Only one profile per protein is allowed.

- `modules`:

  A named list of `MoBiModule` objects from which to create the
  simulation. The order of modules defines the order in which the
  modules will be combined to a simulation! When setting the modules,
  the selection of Initial Conditions and Parameter Values is reset to
  the first available ones in the modules.

- `selectedInitialConditions`:

  A named list with names being the names of the modules, and the values
  the names of Initial Conditions Building Blocks

- `selectedParameterValues`:

  A named list with names being the names of the modules, and the values
  the names of Parameter Values Building Blocks If the modules for which
  the PV selection is provided are not in the configuration, throw an
  error.

- `partitionCoefficientOverrides`:

  A named list of partition coefficient method overrides. Names are
  molecule names, values are the full calculation method names.
  Read-only; use `setPartitionCoefficientMethods()` to modify.

- `cellularPermeabilityOverrides`:

  A named list of cellular permeability method overrides. Names are
  molecule names, values are the full calculation method names.
  Read-only; use `setCellularPermeabilityMethods()` to modify.

- `settings`:

  A `SimulationSettings` object defining the simulation settings. If no
  settings are provided, default settings will be used upon simulation
  creation. Individual properties within the `SimulationSettings` object
  are read-only, but the entire settings object can be replaced with
  another `SimulationSettings` instance (or set to `NULL` to use
  defaults). Setting the solver settings, output intervals, and output
  selections must be done after creating the simulation from the
  configuration.

## Methods

### Public methods

- [`SimulationConfiguration$new()`](#method-SimulationConfiguration-new)

- [`SimulationConfiguration$setPartitionCoefficientMethods()`](#method-SimulationConfiguration-setPartitionCoefficientMethods)

- [`SimulationConfiguration$setCellularPermeabilityMethods()`](#method-SimulationConfiguration-setCellularPermeabilityMethods)

- [`SimulationConfiguration$print()`](#method-SimulationConfiguration-print)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    SimulationConfiguration$new(
      modules,
      individual = NULL,
      expressionProfiles = NULL,
      selectedInitialConditions = NULL,
      selectedParameterValues = NULL,
      settings = NULL
    )

#### Arguments

- `modules`:

  A list of `MoBiModule` objects from which to create in simulation. The
  order of modules defines the order in which the modules will be
  combined to a simulation!

- `individual`:

  Optional, an individual building block

- `expressionProfiles`:

  Optional, a list of expression profiles to apply to the simulation.

- `selectedInitialConditions`:

  By default, the first Initial Conditions (IC) building block (BB) of
  each module will be selected. If a module has multiple IC BBs, it is
  possible to specify which IC BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the IC BB. By explicitly setting the value for a specific module to
  `NULL`, no IC BB from the specified module will be applied. If the
  list contains a module name that is not part of the provided modules,
  it will be ignored.

- `selectedParameterValues`:

  By default, the first Parameter Values (PV) building block (BB) of
  each module will be selected. If a module has multiple PV BBs, it is
  possible to specify which PV BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the PV BB. By explicitly setting the value for a specific module to
  `NULL`, no PV BB from the specified module will be applied. If the
  list contains a module name that is not part of the provided modules,
  it will be ignored.

- `settings`:

  Optional, a `SimulationSettings` object defining the simulation
  settings. If no settings are provided, default settings will be used
  upon simulation creation.

------------------------------------------------------------------------

### Method `setPartitionCoefficientMethods()`

Sets the method used for calculation of partition coefficients. This
method can be used to specify the partition coefficient method for each
molecule used in the modules. If not specified, the method set in the
Molecules Building Block of the last module in which the molecule is
present will be used.

#### Usage

    SimulationConfiguration$setPartitionCoefficientMethods(
      moleculeName,
      methodName
    )

#### Arguments

- `moleculeName`:

  The name of the molecule for which to set the partition coefficient
  method.

- `methodName`:

  The name of the method to use for calculation of partition
  coefficients for the specified molecule. Should be one of the
  `PartitionCoefficientMethods` enum values. Use `NULL` to remove an
  existing override.

------------------------------------------------------------------------

### Method `setCellularPermeabilityMethods()`

Sets the method used for calculation of cellular permeabilities. This
method can be used to specify the cellular permeability method for each
molecule used in the modules. If not specified, the method set in the
Molecules Building Block of the last module in which the molecule is
present will be used.

#### Usage

    SimulationConfiguration$setCellularPermeabilityMethods(
      moleculeName,
      methodName
    )

#### Arguments

- `moleculeName`:

  The name of the molecule for which to set the cellular permeability
  method.

- `methodName`:

  The name of the method to use for calculation of cellular
  permeabilities for the specified molecule. Should be one of the
  `CellularPermeabilityMethods` enum values. Use `NULL` to remove an
  existing override.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    SimulationConfiguration$print(...)

#### Arguments

- `...`:

  Rest arguments.
