# ospsuite 12.0 (development)

## Breaking Changes

  - The single argument of the `getBaseUnit()` function is now named
    `quantityOrDimension` (was `dimension` previously). It can now use objects
    of class `Quantity` as inputs, to be consistent with the `toBaseUnit()`
    function.

## Major Changes

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
    default.
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
  - `DataCombined` gets a new method `setDataTypes()` to change data types (observed
  or simulated) for existing data sets. This method is useful when you want to
  enforce a certain data type, e.g., for adding artificial data set as simulated results.
  - New function `setOutputs()` to change outputs of `SimulationBatch` objects.
  It combines `clearOutputs()` and `setOutputs()` in one function.
  

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
