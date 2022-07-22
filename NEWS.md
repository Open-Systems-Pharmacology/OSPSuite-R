# ospsuite 12.0 (development version)

## New features

* Adds new visualization functions:

    - `plotObservedVsSimulated()` for observed versus simulated data scatter plot.
    - `plotResidualsVsTime()` for time versus residuals data scatter plot.
    - `plotResidualsVsSimulated()` for simulated versus residuals data scatter plot.

## Major Changes

* The class `SimulationBatch` gets a new property `id`.

* The output of `runSimulationBatches()` is now a named list with names being 
  the ids of `SimulationBatch`.
    
# ospsuite 11.0.123

## New features

* Adds new `DataCombined` class that combines observed data (`DataSet`) and
  simulated data (`SimulationResults`) into a single object. Especially
  important is the `$toDataFrame()` method of this object that returns a data
  frame containing combined data from observed and simulated data, which can be
  further used for visualizations or other analysis.

* Adds new visualization functions:

    - `plotIndividualTimeProfile()` and `plotPopulationTimeProfile()` to create 
      time-profile plots.

* Adds a new class `DefaultPlotConfiguration` to provide plot configurations for 
  plotting functions.

* Adds `simulationResultsToDataFrame()` function to convert `SimulationResults`
  objects into a data frame.

* All `*ToDataFrame()` functions also get `*ToTibble()` variants to extract a
  tibble data frame instead of a classical data frame.

## Major Changes

* Following functions are marked for deprecation and will be removed in future
  releases:

    - `pkAnalysesAsDataFrame()` is now `pkAnalysesToDataFrame()`

    - `populationAsDataFrame()` is now `populationToDataFrame()`

## Breaking changes

* Signature for `toUnit` function has changed. `molWeight` is now the fifth,
  while `sourceUnit` is the fourth parameter (#837).

* `DataImporterConfiguration`: fields `timeUnitFromColumn` and
  `measurementUnitFromColumn` are renamed to `isTimeUnitFromColumn` and
  `isMeasurementUnitFromColumn`

* The constructor for `DataImporterConfiguration` does not accept a path to a
  configuration file any more. Use the function
  `loadDataImporterConfiguration()` to create a configuration object from a
  file.

* The constructor for `DataSet` objects requires a `name` argument.

* All `validate*()` helper functions and `"%||%"` infix operators are no longer
  exported. Please have a look at the new utility package, from which these
  functions are now exported:
  <https://www.open-systems-pharmacology.org/OSPSuite.RUtils/>.

* The `pkAnalysesAsDataFrame()` function changes column data types for
  `QuantityPath`, `Parameter`, and `Unit` from `factor` to `character`
  (https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/673).

# ospsuite 10.0

Version compatible with the OSPSuite V10.

## New features

* [Enable Batch Simulation Run](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/444)
* [Export of population to CSV](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/423)
* [Support aging data for population calculation](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/295)
* [Enums to look up units and dimensions](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/478)
* [Support R4](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/531)
* [extend setParameterValuesByPath function](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/541)
* [DataSet: load from PKML](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/575)
* [Efficient calculations](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/efficient-calculations.html)


## Bug fixes

* [Cannot calculate PK-Parameters of a population when one individual simulation failed](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/436)
* [Usability issue when creating a population](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/473)
* [Unnecessary Debug output in some plot tasks](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/503)
* [uniqueEntities: Error when passing only one entity](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/515)
* [R-Toolbox bug? getAllParametersMatching()](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/428)
* [Add CL parameter to the list of StandardPKPArameters](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/582)
* [toBaseUnit and toUnit should support null as input (and return null)](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/583)
* [ospsuite::toBaseUnit should be case insensitive](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/614)
* [Integer variables not handled by some functions](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/553)


# ospsuite 9.0

* Version compatible with the OSPSuite V9.
* Initial Release
