# Export simulation PKMLs for given `individualIds`. Each pkml file will contain the original simulation updated with parameters of the corresponding individual.

Export simulation PKMLs for given `individualIds`. Each pkml file will
contain the original simulation updated with parameters of the
corresponding individual.

## Usage

``` r
exportIndividualSimulations(
  population,
  individualIds,
  outputFolder,
  simulation
)
```

## Arguments

- population:

  A population object typically loaded with `loadPopulation`

- individualIds:

  Ids of individual (single value or array) to export

- outputFolder:

  Folder where the individual simulations will be exported. File format
  will be `simulationName_individualId`

- simulation:

  Simulation uses to generate PKML files

## Value

An array containing the path of all exported simulations.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

popPath <- system.file("extdata", "simple_pop.csv", package = "ospsuite")
population <- loadPopulation(popPath)

exportIndividualSimulations(population, c(1, 2), tempdir(), sim)
#> [1] "/tmp/Rtmpx5sMCG/Simple_1.pkml" "/tmp/Rtmpx5sMCG/Simple_2.pkml"
```
