# Create a new simulation from a simulation configuration

Create a new simulation from a simulation configuration

## Usage

``` r
createSimulation(
  simulationName,
  simulationConfiguration,
  createAllProcessRateParameters = FALSE,
  showWarnings = FALSE
)
```

## Arguments

- simulationName:

  Name of the simulation.

- simulationConfiguration:

  An instance of `SimulationConfiguration` that defines the simulation.

- createAllProcessRateParameters:

  If `TRUE`, process rate parameters will be created for all reactions
  and transport processes.

- showWarnings:

  If `TRUE`, warnings generated during simulation creation will be shown
  as R warnings. Default is `FALSE`.

## Value

A `Simulation` object
