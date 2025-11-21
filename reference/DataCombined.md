# Object combining simulated and observed data

A class for storing simulated and/or observed in a single data frame,
which can be further used in data wrangling or data visualization
pipelines.

Additionally, it allows:

- Grouping different simulated and/or observed datasets.

- Transforming data with given offsets and scale factors.

## Note

The molecular weight (in `molWeight` column) is in `g/mol` units.

## See also

Other data-combined:
[`calculateResiduals()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/calculateResiduals.md),
[`convertUnits()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/convertUnits.md)

## Active bindings

- `names`:

  A vector of unique names of datasets contained in the `DataCombined`
  class instance.

- `groupMap`:

  A data frame specifying which datasets have been grouped together and
  the name and the nature (observed or simulated?) of the data. If a
  dataset was not assigned to any group, this is denoted by `NA` in the
  data frame.

- `dataTransformations`:

  A data frame with offset and scale factor values were specified by the
  user for each dataset.

## Methods

### Public methods

- [`DataCombined$addDataSets()`](#method-DataCombined-addDataSets)

- [`DataCombined$addSimulationResults()`](#method-DataCombined-addSimulationResults)

- [`DataCombined$setGroups()`](#method-DataCombined-setGroups)

- [`DataCombined$setDataTypes()`](#method-DataCombined-setDataTypes)

- [`DataCombined$removeGroupAssignment()`](#method-DataCombined-removeGroupAssignment)

- [`DataCombined$setDataTransformations()`](#method-DataCombined-setDataTransformations)

- [`DataCombined$toDataFrame()`](#method-DataCombined-toDataFrame)

- [`DataCombined$print()`](#method-DataCombined-print)

- [`DataCombined$clone()`](#method-DataCombined-clone)

------------------------------------------------------------------------

### Method `addDataSets()`

Adds observed data.

#### Usage

    DataCombined$addDataSets(dataSets, names = NULL, groups = NULL, silent = FALSE)

#### Arguments

- `dataSets`:

  An instance (or a `list` of instances) of the `DataSet` class.

- `names`:

  A string or a `list` of strings assigning new names. These new names
  can be either for renaming `DataSet` objects, or for renaming
  quantities/paths in `SimulationResults` object. If an entity is not to
  be renamed, this can be specified as `NULL`. E.g., in
  `names = list("oldName1" = "newName1", "oldName2" = NULL)`), dataset
  with name `"oldName2"` will not be renamed. The list can either be
  named or unnamed. Names act as unique identifiers for data sets in the
  `DataCombined` object and, therefore, duplicate names are not allowed.

- `groups`:

  A string or a list of strings specifying group name corresponding to
  each data set. If an entry within the list is `NULL`, the
  corresponding data set is not assigned to any group (and the
  corresponding entry in the `group` column will be an `NA`). If
  provided, `groups` must have the same length as `dataSets` and/or
  `simulationResults$quantityPath`. If no grouping is specified for any
  of the dataset, the column `group` in the data frame output will be
  all `NA`.

- `silent`:

  A binary flag showing if warnings should be triggered when data sets
  are overwritten in the `DataCombined` object

#### Returns

`DataCombined` object containing observed data.

------------------------------------------------------------------------

### Method `addSimulationResults()`

Add simulated data using instance of `SimulationResults` class.

#### Usage

    DataCombined$addSimulationResults(
      simulationResults,
      quantitiesOrPaths = NULL,
      population = NULL,
      individualIds = NULL,
      names = NULL,
      groups = NULL,
      silent = FALSE
    )

#### Arguments

- `simulationResults`:

  Object of type `SimulationResults` produced by calling
  `runSimulations` on a `Simulation` object.

- `quantitiesOrPaths`:

  Quantity instances (element or vector) typically retrieved using
  `getAllQuantitiesMatching` or quantity path (element or vector of
  strings) for which the results are to be returned. (optional) When
  providing the paths, only absolute full paths are supported (i.e., no
  matching with '\*' possible). If quantitiesOrPaths is `NULL` (default
  value), returns the results for all output defined in the results.

- `population`:

  population used to calculate the `simulationResults` (optional). This
  is used only to add the population covariates to the resulting data
  table.

- `individualIds`:

  `numeric` IDs of individuals for which the results should be
  extracted. By default, all individuals from the results are
  considered. If the individual with the provided ID is not found, the
  ID is ignored.

- `names`:

  A string or a `list` of strings assigning new names. These new names
  can be either for renaming `DataSet` objects, or for renaming
  quantities/paths in `SimulationResults` object. If an entity is not to
  be renamed, this can be specified as `NULL`. E.g., in
  `names = list("oldName1" = "newName1", "oldName2" = NULL)`), dataset
  with name `"oldName2"` will not be renamed. The list can either be
  named or unnamed. Names act as unique identifiers for data sets in the
  `DataCombined` object and, therefore, duplicate names are not allowed.

- `groups`:

  A string or a list of strings specifying group name corresponding to
  each data set. If an entry within the list is `NULL`, the
  corresponding data set is not assigned to any group (and the
  corresponding entry in the `group` column will be an `NA`). If
  provided, `groups` must have the same length as `dataSets` and/or
  `simulationResults$quantityPath`. If no grouping is specified for any
  of the dataset, the column `group` in the data frame output will be
  all `NA`.

- `silent`:

  A binary flag showing if warnings should be triggered when data sets
  are overwritten in the `DataCombined` object

#### Returns

`DataCombined` object containing simulated data.

------------------------------------------------------------------------

### Method `setGroups()`

Adds grouping information to (observed and/or simulated) datasets.

#### Usage

    DataCombined$setGroups(names, groups)

#### Arguments

- `names`:

  A list of dataset names which need to be grouped. Note that if you
  have specified new `names` while adding datasets (using
  `$addDataSets()` and `$addSimulationResults()` methods), you will need
  to use these new names to specify group assignment. The same dataset
  can't be assigned to two different groupings in the *same*
  `$setGroups()` call. In other words, elements of `names` argument
  should be unique.

- `groups`:

  A list specifying which datasets belong to which group(s). Please note
  that the order in which groups are specified should match the order in
  which datasets were specified for `names` parameter. For example, if
  data sets are named `"x"`, `"y"`, `"z"`, and the desired groupings for
  them are, respectively, `"a"`, `"b"`, this can be specified as
  `names = list("x", "y"), groups = list("a", "b")`. Datasets for which
  no grouping is to be specified, can be left out of the `groups`
  argument. The column `group` in the data frame output will be `NA` for
  such datasets. If you wish to remove an *existing* grouping assignment
  for a given dataset, you can specify it as following: `list("x" = NA)`
  or `list("x" = NULL)`. This will not change any of the other
  groupings.

#### Returns

`DataCombined` object with grouped datasets.

------------------------------------------------------------------------

### Method `setDataTypes()`

set the type of data (observed or simulated) for datasets.

#### Usage

    DataCombined$setDataTypes(names, dataTypes)

#### Arguments

- `names`:

  a character vector of dataset names which dataTypes need to be
  changed.

- `dataTypes`:

  a character vector of dataTypes (`"observed"` or `"simulated"`) to be
  assigned to the datasets (in order of `names`.

#### Returns

`DataCombined` object with modified dataTypes datasets.

------------------------------------------------------------------------

### Method `removeGroupAssignment()`

Remove existing groupings for (observed and/or simulated) datasets.

#### Usage

    DataCombined$removeGroupAssignment(names)

#### Arguments

- `names`:

  A list of dataset names whose group assignment needs to be removed.
  Note that if you have specified new `names` while adding datasets
  (using `$addDataSets()` and `$addSimulationResults()` methods), you
  will need to use these new names to specify group assignment. The
  elements of `names` argument should be unique.

#### Returns

`DataCombined` object with updated group assignments.

------------------------------------------------------------------------

### Method `setDataTransformations()`

Transform raw data with required offsets and scale factors.

#### Usage

    DataCombined$setDataTransformations(
      forNames = NULL,
      xOffsets = 0,
      yOffsets = 0,
      xScaleFactors = 1,
      yScaleFactors = 1,
      reset = FALSE
    )

#### Arguments

- `forNames`:

  A list of names specifying which observed datasets and/or paths in
  simulated dataset to transform with the specified transformations.
  Default is `NULL`, i.e., the transformations, if any specified, will
  be applied to all rows of the data frame.

- `xOffsets, yOffsets, xScaleFactors, yScaleFactors`:

  Either a single numeric value or a list of numeric values specifying
  offsets and scale factors to apply to raw values. The default offset
  is `0`, while default scale factor is `1`, i.e., the data will not be
  modified. If a list is specified, it should be the same length as
  `forNames` argument.

- `reset`:

  IF `TRUE`, only data transformations that are specified will be
  retained. Not specified transformations will be reset to their
  defaults. Default behavior is `FALSE`, e.g., setting only `xOffsets`
  will not reset `xScaleFactors` if those have been set previously.

#### Details

A data frame with respective raw quantities transformed using specified
offset and scale factor values.

- For X and Y variables: `newValue = (rawValue + offset) * scaleFactor`

- For error term: `newErrorValue = rawErrorValue * scaleFactor`

------------------------------------------------------------------------

### Method `toDataFrame()`

A method to extract a tibble data frame of simulated and/or observed
data (depending on instances of which classes have been added to the
object).

Note that the order in which you enter different object doesn't matter
because the returned data frame is arranged alphabetically by dataset
name.

#### Usage

    DataCombined$toDataFrame()

#### Returns

In the returned tibble data frame, the following columns will always be
present:

name - group - dataType - xValues - xDimension - xUnit - yValues -
yErrorValues - yDimension - yUnit - yErrorType - yErrorUnit - molWeight

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console.

#### Usage

    DataCombined$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataCombined$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# simulated data
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simResults <- runSimulations(sim)[[1]]
outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"

# observed data
obsData <- lapply(
  c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
  function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
)
names(obsData) <- lapply(obsData, function(x) x$name)


# Create a new instance of `DataCombined` class
myDataCombined <- DataCombined$new()

# Add simulated results
myDataCombined$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPath,
  groups = "Aciclovir PVB"
)

# Add observed data set
myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

# Looking at group mappings
myDataCombined$groupMap
#> # A tibble: 2 × 3
#>   name                                                            group dataType
#>   <chr>                                                           <chr> <chr>   
#> 1 Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Ve… Acic… simulat…
#> 2 Vergin 1995.Iv                                                  Acic… observed

# Looking at the applied transformations
myDataCombined$dataTransformations
#> # A tibble: 2 × 5
#>   name                             xOffsets yOffsets xScaleFactors yScaleFactors
#>   <chr>                               <dbl>    <dbl>         <dbl>         <dbl>
#> 1 Organism|PeripheralVenousBlood|…        0        0             1             1
#> 2 Vergin 1995.Iv                          0        0             1             1

# Accessing the combined data frame
myDataCombined$toDataFrame()
#> # A tibble: 504 × 27
#>    IndividualId xValues name           yValues xDimension xUnit yDimension yUnit
#>           <int>   <dbl> <chr>            <dbl> <chr>      <chr> <chr>      <chr>
#>  1            0       0 Organism|Peri…    0    Time       min   Concentra… µmol…
#>  2            0       1 Organism|Peri…    3.25 Time       min   Concentra… µmol…
#>  3            0       2 Organism|Peri…    9.10 Time       min   Concentra… µmol…
#>  4            0       3 Organism|Peri…   15.0  Time       min   Concentra… µmol…
#>  5            0       4 Organism|Peri…   20.7  Time       min   Concentra… µmol…
#>  6            0       5 Organism|Peri…   26.2  Time       min   Concentra… µmol…
#>  7            0       6 Organism|Peri…   31.4  Time       min   Concentra… µmol…
#>  8            0       7 Organism|Peri…   36.4  Time       min   Concentra… µmol…
#>  9            0       8 Organism|Peri…   41.1  Time       min   Concentra… µmol…
#> 10            0       9 Organism|Peri…   45.5  Time       min   Concentra… µmol…
#> # ℹ 494 more rows
#> # ℹ 19 more variables: molWeight <dbl>, dataType <chr>, yErrorValues <dbl>,
#> #   yErrorType <chr>, yErrorUnit <chr>, lloq <dbl>, Source <chr>, File <chr>,
#> #   Sheet <chr>, Molecule <chr>, Species <chr>, Organ <chr>, Compartment <chr>,
#> #   `Study Id` <chr>, Gender <chr>, Dose <chr>, Route <chr>,
#> #   `Patient Id` <chr>, group <chr>
```
