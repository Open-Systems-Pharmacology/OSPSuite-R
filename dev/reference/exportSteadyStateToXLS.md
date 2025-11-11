# Export steady-state to Excel in the format that can be imported in MoBi.

Export steady-state to Excel in the format that can be imported in MoBi.

## Usage

``` r
exportSteadyStateToXLS(
  simulation,
  quantitiesPaths = NULL,
  resultsXLSPath = "",
  steadyStateTime = NULL,
  ignoreIfFormula = TRUE,
  lowerThreshold = 1e-15,
  simulationRunOptions = NULL
)
```

## Arguments

- simulation:

  A `Simulation` object for which the steady-state will be simulated. In
  contrast to
  [`getSteadyState()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getSteadyState.md),
  only one simulation is supported.

- quantitiesPaths:

  List of quantity paths (molecules and/or parameters) for which the
  steady-state will be simulated. If `NULL` (default), all molecules and
  state variable parameters are considered. The same list is applied for
  all simulations.

- resultsXLSPath:

  Path to the xls-file where the results will be written to. If the file
  does not exist, a new file is created. If no path is provided, the
  file will be created in the same directory where the model file is
  located. The name of the file will be `<SimulationFileName>_SS.xlsx`.

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
  values below this value are considered as numerical noise and replaced
  by 0. If `lowerThreshold` is `NULL`, no cut-off is applied. Default
  value is 1e-15.

- simulationRunOptions:

  Optional instance of a `SimulationRunOptions` used during the
  simulation run.

## Value

An `openxlsx` workbook object.

## Details

Simulates a given model to its steady-state and creates an Excel-file
with the end values of molecules amounts in all containers and parameter
values that have a right-hand-side (state variable parameters). The
excel file contains two sheets - one for the molecules and one for the
parameters.
