# Get the steady-state values of species and state variable parameters.

Get the steady-state values of species and state variable parameters.

## Usage

``` r
getSteadyState(
  simulations,
  quantitiesPaths = NULL,
  steadyStateTime = NULL,
  ignoreIfFormula = TRUE,
  lowerThreshold = 1e-15,
  simulationRunOptions = NULL
)
```

## Arguments

- simulations:

  `Simulation` object or a list or vector of `Simulation` objects to
  simulate. List or vector can be named (names must be uniques), in
  which case the names will reused in the output list. If not named, the
  output list will use simulation ids for names.

- quantitiesPaths:

  List of quantity paths (molecules and/or parameters) for which the
  steady-state will be simulated. If `NULL` (default), all molecules and
  state variable parameters are considered. The same list is applied for
  all simulations.

- steadyStateTime:

  Simulation time (minutes). In `NULL` (default), the default simulation
  time is the start time of the last application plus three days. The
  simulated time must be long enough for the system to reach a
  steady-state. Either a single value (will be applied for all
  simulations), or a list of values specific for each simulation. In
  latter case, must have equal size as `simulations`. When providing a
  list, `NULL` is allowed to calculate the time based on the last
  application.

- ignoreIfFormula:

  If `TRUE` (default), species and parameters with initial values
  defined by a formula are not included.

- lowerThreshold:

  Numerical value (in default unit of the output). Any steady-state
  values with absolute value below this threshold are considered as
  numerical noise and replaced by 0 (i.e., values in the interval
  `[-lowerThreshold, lowerThreshold]`). If `lowerThreshold` is `NULL`,
  no cut-off is applied. Default value is 1e-15.

- simulationRunOptions:

  Optional instance of a `SimulationRunOptions` used during the
  simulation run.

## Value

A named list, where the names are the IDs of the simulations and the
entries are lists containing `paths` and their `values` at the end of
the simulation.

## Details

The steady-state is considered to be the last values of the molecules
amounts and state variable parameters in the simulation with
sufficiently long simulation time, i.e., where the rates of the
processes do not (significantly) change. The steady-state is NOT
analytically calculated or estimated in any other way than simulating
for the given time.

## Examples

``` r
simPath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
steadyState <- getSteadyState(simulations = sim)
# Set initial values for steady-state simulations
setQuantityValuesByPath(
  quantityPaths = steadyState[[sim$id]]$paths,
  values = steadyState[[sim$id]]$values, simulation = sim
)
```
