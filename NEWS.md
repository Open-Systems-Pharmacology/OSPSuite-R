# ospsuite 12.3.1

## Minor improvements and bug fixes

- Fixed tests expectations for better compatibility with latest version of
  `{ospsuite.utils}` package. #1570
- Revised "get started" (https://www.open-systems-pharmacology.org/OSPSuite-R/articles/ospsuite.html) documentation page to make it more accesible to new users. #1571

# ospsuite 12.3.0

## Breaking changes

- ´{ospsuite}` now requires `{ospsuite.utils}` version \>= 1.7.0.
- Classes `SimulationBatchRunValues` and `SimulationBatchOptions` are not exported any more.
They should not be used directly.

## Major changes
- `DataImporterConfiguration` now supports getting and setting of the Lower Limit 
of Quantification (LLOQ) column name via
a new field `lloqColumn`. If the column name is not set (value `NULL`), LLOQ values
will be imported from the measurement column if values are written in the form '< xxx' (e.g., '<0.001').
Otherwise, the values will be imported from the specified column (\#722)

## Minor improvements and bug fixes

- Added `showLegendPerDataset` parameter to `plotIndividualTimeProfile()` and 
`plotPupulationTimeProfile()` to optionally display separate legend entries for 
each dataset. This is experimental.
- Improved print outputs for all classes
- Classes do not inherit from the deprecated `Printable` class from the `{ospsuite.utils}` package.
- Print methods for all classes are now implemented using the `ospPrint\*` functions 
introduced in version 1.6.2. of the `{ospsuite.utils}` package.
- `ParameterRange$printValue()` is deprecated. Use `getPrintValue()` in conjunction with a print method of your choice.
-  Private method `Quantity$printQuantity()` removed. `Quantity$printValue()` and `Quantity$printQuantityValue()` are 
deprecated. Use `getPrintValue()` in conjunction with a print method of your choice.
- `SnapshotParameter$printValue()` is deprecated. Use `getPrintValue()` in conjunction with a print method of your choice.
- Added snapshot tests for all print methods.
- `calculateResiduals()` for `DataCombined` now supports full pairwise residual 
computation between multiple observed and simulated datasets within a group.
- The `name` property of a `Simulation` can now be changed (#1245)
- `calculateResiduals`  now handles single-point simulated datasets via direct 
x-value matching instead of interpolation. Unmatched observed x-values return 
`NA` (#1559).

# ospsuite 12.2.0

## Major Changes

- New `runSimulationsFromSnapshot()` to run simulations from `.json` snapshots files,
- New `convertSnapshot()` to convert project snapshots between `.json` and `.pksim5` files.
- New `getMolWeightFor()` to retrieve molecular weight for molecule of `Quantity`.

# ospsuite 12.1.0

## Major Changes

  - The package fully supports Linux.
  - The package loses dependency `{rClr}` and gains `{rSharp}`.
  - The package does not require a local installation of PK-Sim any more. All required 
  dependencies are shipped with the package.


# ospsuite 12.0.0


## Breaking Changes

  - The single argument of the `getBaseUnit()` function is now named
    `quantityOrDimension` (was `dimension` previously). It can now use objects
    of class `Quantity` as inputs, to be consistent with the `toBaseUnit()`
    function.

## Major Changes

  - The package gains `{openxlsx}` and `{lifecyle}` dependencies.
  - Added a function `getSteadyState()` to calculate steady state values for
    simulations. This function is of particular use for models of endogenous
    substrates, where changing a parameter value (e.g., the production rate)
    will change the steady-state values of the substrate. The steady-state is
    considered to be the last values of the molecules amounts and state variable
    parameters in the simulation with sufficiently long simulation time, i.e.,
    where the rates of the processes do not (significantly) change. The
    steady-state is NOT analytically calculated or estimated in any other way
    than simulating for the given time.
  - Added a function `exportSteadyStateToXLS()` to export steady state values
    calculated for one simulation using the `getSteadyState()` to an Excel file
    that can be imported in MoBi.
  - Lower limit of quantification (LLOQ) is plotted as dotted lines for
    `plotIndividualTimeProfile()` and `plotPopulationTimeProfile()`.
  - `plotPopulationTimeProfile()` provides two new aggregations methods:
    `arithmetic` and `geometric` average and the ability to chose the number of
    standard deviations to display around the mean through the `nsd` argument.
  - Function `calculateResiduals()` uses natural logarithm (`log()`) when
    calculating residuals with `scaling = "log"` as opposed to `log10()` as it
    was done before. This also affects figures created by
    `plotObservedVsSimulated()`, `plotResidualsVsSimulated()`, and
    `plotResidualsVsTime()`. (\#1091, \#1087).
  - `plotPopulationTimeProfile()` has a new `aggregation` argument that allow
    the user to choose between quantiles, arithmetic and geometric aggregations
    of y.


## Minor improvements and bug fixes
  
  - Named `Simulation` lists can be passed to `runSimulations()` so that the
    results are returned as a named list using the same names instead of the
    simulation id. If no name is provided, the simulation id is used as before.
    (\#1383)
  - The order of columns of the data frame returned by
    `DataCombined$toDataFrame()` has changed.
  - `DataCombined$toDataFrame()`
    shows the data in the order as the data sets were added and not
    alphabetically sorted.
  - `DataCombined$removeGroupAssignment()` does not show a warning if specified name is
not present in the `DataCombined`.  
  - `DataCombined$removeGroupAssignment()`
    does not produce an error if specified names are not unique.
  - Fixed Passing font size options from plotConfiguration objects to tlf
    objects (\#1198)
  - Legend now have transparent background by default and can be customized
    through new `DefaultPlotConfiguration` fields (\#1216)
  - `plotObsVsPred()` now have foldDistance argument set to `NULL` by default
    (previously 2). The function will not add any fold lines on the plot by
    default but display identity line. Set this argument to `FALSE` to not draw
    any lines.
  - `DefaultPlotConfiguration` has a new setting: `displayLLOQ` (default TRUE)
    to control plotting of LLOQ lines.
  - `DefaultPlotConfiguration`'s `xLimits` and `yLimits` arguments are
    deprecated and replaced by `xAxisLimits` and `yAxisLimits`. Use them to zoom
    in the plot while preserving all data points. Use `xValuesLimits` and
    `yValuesLimits` to filter out data point outside of these range. More
    detailed explanations
    [here](https://ggplot2.tidyverse.org/reference/coord_cartesian.html#ref-examples).
  - `addSimulationResults` and `addDataSets` methods of the `DataCombined` class
    now support an optional `silent` argument which silences the checks for data
    set names. If you expect to replace data sets in `DataCombined` objects
    repeatedly, consider switching the parameter from the default `FALSE` value
    to `TRUE`.
  - `simulationResultsToDataFrame()` is faster than before (\#1317, @Felixmil).
  - `DataCombined` gets a new method `setDataTypes()` to change data types
    (observed or simulated) for existing data sets. This method is useful when
    you want to enforce a certain data type, e.g., for adding artificial data
    set as simulated results.
  - New function `setOutputs()` to change outputs of `SimulationBatch` objects.
    It combines `clearOutputs()` and `setOutputs()` in one function.
  - New simulations life cycle flowcharts in the
    `vignette("efficient-calculations")` vignette.
  - New internal function `.setEndSimulationTime()` to set the end time of the
    simulation. The function will either extend or shorten the simulation time to the specified end time.
  - Using `runSimulation()` is now soft deprecated in favor of `runSimulations()`.
  

# ospsuite 11.1.197

## Major Changes

  - Adds new visualization functions:

    - `plotObservedVsSimulated()` for observed versus simulated data scatter plot.
    - `plotResidualsVsTime()` for time versus residuals data scatter plot.
    - `plotResidualsVsSimulated()` for simulated versus residuals data scatter plot.

  - Adds new helper functions to work with `DataCombined` objects:

    - `convertUnits()` to convert datasets in `DataCombined` to common units.
    - `calculateResiduals()` to calculate residuals for datasets in `DataCombined`.

  - The class `SimulationBatch` gets a new property `id`.

  - The output of `runSimulationBatches()` is now a named list with names being
    the ids of `SimulationBatch`.
  
  - `calculateResiduals()` now uses `log(base = 10)` for calculation of
    residuals in logarithmic scale instead if `log(base = exp(1))`
  - `calculateResiduals()` does also return residuals for entries where
    simulated or observed value is 0 in logarithmic scale. These values were
    ignored in previous versions. If the observed or simulated value is zero or
    negative, it is replaced by an arbitrary small value
    `getOSPSuiteSetting("LOG_SAFE_EPSILON")` (1e-20 by default).
  
## Minor improvements and bug fixes

  - `SimulationBatch$addRunValues()` will throw an error when any start value is
    `NaN`.
  - `SimulatioBatch` gets methods `getVariableParameters()` and
    `getVariableMolecules()` that return list of parameter resp. molecule paths
    that are defined variable.
    
# ospsuite 11.0.123

## Breaking changes

  - Signature for `toUnit` function has changed. `molWeight` is now the fifth,
    while `sourceUnit` is the fourth parameter (\#837).

  - `DataImporterConfiguration`: fields `timeUnitFromColumn` and
    `measurementUnitFromColumn` are renamed to `isTimeUnitFromColumn` and
    `isMeasurementUnitFromColumn`

  - The constructor for `DataImporterConfiguration` does not accept a path to a
    configuration file any more. Use the function
    `loadDataImporterConfiguration()` to create a configuration object from a
    file.

  - The constructor for `DataSet` objects requires a `name` argument.

  - All `validate*()` helper functions and `"%||%"` infix operators are no
    longer exported. Please have a look at the new utility package, from which
    these functions are now exported:
    <https://www.open-systems-pharmacology.org/OSPSuite.RUtils/>.

  - The `pkAnalysesAsDataFrame()` function changes column data types for
    `QuantityPath`, `Parameter`, and `Unit` from `factor` to `character`
    (\#673).
    
## Major Changes

  - Adds new `DataCombined` class that combines observed data (`DataSet`) and
    simulated data (`SimulationResults`) into a single object. Especially
    important is the `$toDataFrame()` method of this object that returns a data
    frame containing combined data from observed and simulated data, which can
    be further used for visualizations or other analysis.

  - Adds new visualization functions:

  - `plotIndividualTimeProfile()` and `plotPopulationTimeProfile()` to create
    time-profile plots.

  - Adds a new class `DefaultPlotConfiguration` to provide plot configurations
    for plotting functions.

  - Adds `simulationResultsToDataFrame()` function to convert
    `SimulationResults` objects into a data frame.

  - All `*ToDataFrame()` functions also get `*ToTibble()` variants to extract a
    tibble data frame instead of a classical data frame.

  - Following functions are marked for deprecation and will be removed in future
    releases:

    - `pkAnalysesAsDataFrame()` is now `pkAnalysesToDataFrame()`
    - `populationAsDataFrame()` is now `populationToDataFrame()`


# ospsuite 10.0

Version compatible with the OSPSuite V10.

## Major Changes

  - Enable Batch Simulation Run (\#444).
  - Export of population to CSV (\#423).
  - Support aging data for population calculation (\#295).
  - Enums to look up units and dimensions (\#478).
  - Support R4 (\#531).
  - extend setParameterValuesByPath function (\#541).
  - DataSet: load from PKML (\#575).
  - Efficient calculations, see [this
    vignette](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/efficient-calculations.html).


## Minor improvements and bug fixes

  - Cannot calculate PK-Parameters of a population when one individual
    simulation failed (\#436).
  - Usability issue when creating a population (\#473).
  - Unnecessary Debug output in some plot tasks (\#503).
  - uniqueEntities: Error when passing only one entity (\#515).
  - R-Toolbox bug? getAllParametersMatching(). (\#428).
  - Add CL parameter to the list of StandardPKPArameters (\#582).
  - toBaseUnit and toUnit should support null as input (and return null).
    (\#583).
  - ospsuite::toBaseUnit should be case insensitive (\#614).
  - Integer variables not handled by some functions (\#553).


# ospsuite 9.0

  - Version compatible with the OSPSuite V9.
  - Initial Release
