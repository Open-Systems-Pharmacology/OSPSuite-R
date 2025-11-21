# Converts a `SimulationResults` objects to a data.frame

Converts a `SimulationResults` objects to a data.frame

## Usage

``` r
simulationResultsToDataFrame(
  simulationResults,
  quantitiesOrPaths = NULL,
  population = NULL,
  individualIds = NULL
)

simulationResultsToTibble(
  simulationResults,
  quantitiesOrPaths = NULL,
  population = NULL,
  individualIds = NULL
)
```

## Arguments

- simulationResults:

  Object of type `SimulationResults` produced by calling
  `runSimulations` on a `Simulation` object.

- quantitiesOrPaths:

  Quantity instances (element or vector) typically retrieved using
  `getAllQuantitiesMatching` or quantity path (element or vector of
  strings) for which the results are to be returned. (optional) When
  providing the paths, only absolute full paths are supported (i.e., no
  matching with '\*' possible). If quantitiesOrPaths is `NULL` (default
  value), returns the results for all output defined in the results.

- population:

  population used to calculate the `simulationResults` (optional). This
  is used only to add the population covariates to the resulting data
  table.

- individualIds:

  `numeric` IDs of individuals for which the results should be
  extracted. By default, all individuals from the results are
  considered. If the individual with the provided ID is not found, the
  ID is ignored.

## Value

SimulationResults object as data.frame with columns IndividualId, Time,
paths, simulationValues, unit, dimension, TimeUnit.

## Examples

``` r
library(ospsuite)

simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

# Running an individual simulation
# results is an instance of `SimulationResults`
results <- runSimulations(sim)[[1]]

# convert to a dataframe
simulationResultsToDataFrame(results)
#>    IndividualId Time            paths simulationValues TimeDimension TimeUnit
#> 1             0    0 Organism|Liver|A        30.000000          Time      min
#> 2             0    0 Organism|Liver|B         5.000000          Time      min
#> 3             0    0       Organism|A        40.000000          Time      min
#> 4             0    0       Organism|B        10.000000          Time      min
#> 5             0   15 Organism|Liver|A        25.820301          Time      min
#> 6             0   15 Organism|Liver|B         9.179699          Time      min
#> 7             0   15       Organism|A        34.427067          Time      min
#> 8             0   15       Organism|B        15.572932          Time      min
#> 9             0   30 Organism|Liver|A        22.223164          Time      min
#> 10            0   30 Organism|Liver|B        12.776835          Time      min
#> 11            0   30       Organism|A        29.630886          Time      min
#> 12            0   30       Organism|B        20.369114          Time      min
#> 13            0   45 Organism|Liver|A        19.129129          Time      min
#> 14            0   45 Organism|Liver|B        15.870872          Time      min
#> 15            0   45       Organism|A        25.505505          Time      min
#> 16            0   45       Organism|B        24.494495          Time      min
#> 17            0   60 Organism|Liver|A        16.467295          Time      min
#> 18            0   60 Organism|Liver|B        18.532705          Time      min
#> 19            0   60       Organism|A        21.956394          Time      min
#> 20            0   60       Organism|B        28.043606          Time      min
#> 21            0   75 Organism|Liver|A        14.176212          Time      min
#> 22            0   75 Organism|Liver|B        20.823788          Time      min
#> 23            0   75       Organism|A        18.901617          Time      min
#> 24            0   75       Organism|B        31.098383          Time      min
#> 25            0   90 Organism|Liver|A        12.203826          Time      min
#> 26            0   90 Organism|Liver|B        22.796175          Time      min
#> 27            0   90       Organism|A        16.271769          Time      min
#> 28            0   90       Organism|B        33.728233          Time      min
#> 29            0  105 Organism|Liver|A        10.505752          Time      min
#> 30            0  105 Organism|Liver|B        24.494247          Time      min
#> 31            0  105       Organism|A        14.007668          Time      min
#> 32            0  105       Organism|B        35.992332          Time      min
#> 33            0  120 Organism|Liver|A         9.043887          Time      min
#> 34            0  120 Organism|Liver|B        25.956112          Time      min
#> 35            0  120       Organism|A        12.058517          Time      min
#> 36            0  120       Organism|B        37.941483          Time      min
#> 37            0  135 Organism|Liver|A         7.785418          Time      min
#> 38            0  135 Organism|Liver|B        27.214582          Time      min
#> 39            0  135       Organism|A        10.380558          Time      min
#> 40            0  135       Organism|B        39.619442          Time      min
#> 41            0  150 Organism|Liver|A         6.702065          Time      min
#> 42            0  150 Organism|Liver|B        28.297935          Time      min
#> 43            0  150       Organism|A         8.936086          Time      min
#> 44            0  150       Organism|B        41.063915          Time      min
#> 45            0  165 Organism|Liver|A         5.769464          Time      min
#> 46            0  165 Organism|Liver|B        29.230536          Time      min
#> 47            0  165       Organism|A         7.692618          Time      min
#> 48            0  165       Organism|B        42.307381          Time      min
#> 49            0  180 Organism|Liver|A         4.966637          Time      min
#> 50            0  180 Organism|Liver|B        30.033363          Time      min
#> 51            0  180       Organism|A         6.622183          Time      min
#> 52            0  180       Organism|B        43.377815          Time      min
#> 53            0  195 Organism|Liver|A         4.275526          Time      min
#> 54            0  195 Organism|Liver|B        30.724474          Time      min
#> 55            0  195       Organism|A         5.700700          Time      min
#> 56            0  195       Organism|B        44.299301          Time      min
#> 57            0  210 Organism|Liver|A         3.680582          Time      min
#> 58            0  210 Organism|Liver|B        31.319418          Time      min
#> 59            0  210       Organism|A         4.907443          Time      min
#> 60            0  210       Organism|B        45.092556          Time      min
#> 61            0  225 Organism|Liver|A         3.168425          Time      min
#> 62            0  225 Organism|Liver|B        31.831575          Time      min
#> 63            0  225       Organism|A         4.224566          Time      min
#> 64            0  225       Organism|B        45.775433          Time      min
#> 65            0  240 Organism|Liver|A         2.727535          Time      min
#> 66            0  240 Organism|Liver|B        32.272465          Time      min
#> 67            0  240       Organism|A         3.636713          Time      min
#> 68            0  240       Organism|B        46.363285          Time      min
#> 69            0  255 Organism|Liver|A         2.347995          Time      min
#> 70            0  255 Organism|Liver|B        32.652004          Time      min
#> 71            0  255       Organism|A         3.130660          Time      min
#> 72            0  255       Organism|B        46.869339          Time      min
#> 73            0  270 Organism|Liver|A         2.021268          Time      min
#> 74            0  270 Organism|Liver|B        32.978733          Time      min
#> 75            0  270       Organism|A         2.695024          Time      min
#> 76            0  270       Organism|B        47.304977          Time      min
#> 77            0  285 Organism|Liver|A         1.740006          Time      min
#> 78            0  285 Organism|Liver|B        33.259995          Time      min
#> 79            0  285       Organism|A         2.320008          Time      min
#> 80            0  285       Organism|B        47.679993          Time      min
#> 81            0  300 Organism|Liver|A         1.497881          Time      min
#> 82            0  300 Organism|Liver|B        33.502117          Time      min
#> 83            0  300       Organism|A         1.997175          Time      min
#> 84            0  300       Organism|B        48.002823          Time      min
#>    dimension unit molWeight
#> 1     Amount µmol        NA
#> 2     Amount µmol        NA
#> 3     Amount µmol        NA
#> 4     Amount µmol        NA
#> 5     Amount µmol        NA
#> 6     Amount µmol        NA
#> 7     Amount µmol        NA
#> 8     Amount µmol        NA
#> 9     Amount µmol        NA
#> 10    Amount µmol        NA
#> 11    Amount µmol        NA
#> 12    Amount µmol        NA
#> 13    Amount µmol        NA
#> 14    Amount µmol        NA
#> 15    Amount µmol        NA
#> 16    Amount µmol        NA
#> 17    Amount µmol        NA
#> 18    Amount µmol        NA
#> 19    Amount µmol        NA
#> 20    Amount µmol        NA
#> 21    Amount µmol        NA
#> 22    Amount µmol        NA
#> 23    Amount µmol        NA
#> 24    Amount µmol        NA
#> 25    Amount µmol        NA
#> 26    Amount µmol        NA
#> 27    Amount µmol        NA
#> 28    Amount µmol        NA
#> 29    Amount µmol        NA
#> 30    Amount µmol        NA
#> 31    Amount µmol        NA
#> 32    Amount µmol        NA
#> 33    Amount µmol        NA
#> 34    Amount µmol        NA
#> 35    Amount µmol        NA
#> 36    Amount µmol        NA
#> 37    Amount µmol        NA
#> 38    Amount µmol        NA
#> 39    Amount µmol        NA
#> 40    Amount µmol        NA
#> 41    Amount µmol        NA
#> 42    Amount µmol        NA
#> 43    Amount µmol        NA
#> 44    Amount µmol        NA
#> 45    Amount µmol        NA
#> 46    Amount µmol        NA
#> 47    Amount µmol        NA
#> 48    Amount µmol        NA
#> 49    Amount µmol        NA
#> 50    Amount µmol        NA
#> 51    Amount µmol        NA
#> 52    Amount µmol        NA
#> 53    Amount µmol        NA
#> 54    Amount µmol        NA
#> 55    Amount µmol        NA
#> 56    Amount µmol        NA
#> 57    Amount µmol        NA
#> 58    Amount µmol        NA
#> 59    Amount µmol        NA
#> 60    Amount µmol        NA
#> 61    Amount µmol        NA
#> 62    Amount µmol        NA
#> 63    Amount µmol        NA
#> 64    Amount µmol        NA
#> 65    Amount µmol        NA
#> 66    Amount µmol        NA
#> 67    Amount µmol        NA
#> 68    Amount µmol        NA
#> 69    Amount µmol        NA
#> 70    Amount µmol        NA
#> 71    Amount µmol        NA
#> 72    Amount µmol        NA
#> 73    Amount µmol        NA
#> 74    Amount µmol        NA
#> 75    Amount µmol        NA
#> 76    Amount µmol        NA
#> 77    Amount µmol        NA
#> 78    Amount µmol        NA
#> 79    Amount µmol        NA
#> 80    Amount µmol        NA
#> 81    Amount µmol        NA
#> 82    Amount µmol        NA
#> 83    Amount µmol        NA
#> 84    Amount µmol        NA
```
