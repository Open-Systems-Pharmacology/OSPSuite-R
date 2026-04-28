# Create a simulation configuration from modules.

This function calls the `SimulationConfiguration` constructor.

## Usage

``` r
createSimulationConfiguration(
  modules,
  individual = NULL,
  expressionProfiles = NULL,
  selectedInitialConditions = NULL,
  selectedParameterValues = NULL,
  settings = NULL
)
```

## Arguments

- modules:

  A list of `MoBiModule` objects from which to create the simulation.
  The order of modules defines the order in which the modules will be
  combined to a simulation!

- individual:

  Optional, an individual building block

- expressionProfiles:

  Optional, a list of expression profiles to apply to the simulation.

- selectedInitialConditions:

  By default, the first Initial Conditions (IC) building block (BB) of
  each module will be selected. If a module has multiple IC BBs, it is
  possible to specify which IC BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the IC BB. If `NULL`, all modules will use the first IC BB, if
  available. When providing a list, the value can also be set to `NULL`,
  which means that no IC BB from the specified module will be selected.

- selectedParameterValues:

  By default, the first Parameter Values (PV) building block (BB) of
  each module will be selected. If a module has multiple PV BBs, it is
  possible to specify which PV BB to apply by providing a named list,
  where the name should be the name of the module and the value the name
  of the PV BB. If `NULL`, all modules will use the first PV BB, if
  available. When providing a list, the value can also be set to `NULL`,
  which means that no PV BB from the specified module will be selected.

- settings:

  Optional, a `SimulationSettings` object defining the simulation
  settings. If no settings are provided, default settings will be used
  upon simulation creation.

## Value

A `SimulationConfiguration` object.
