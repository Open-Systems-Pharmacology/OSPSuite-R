# Creates an population using the PK-Sim Database

Creates an population using the PK-Sim Database

## Usage

``` r
createPopulation(populationCharacteristics)
```

## Arguments

- populationCharacteristics:

  Characteristics of the population to create as an instance of
  `OriginData` that are actually distributed parameters

## Value

An list with three entries:

- `population` An instance of a population object.

- `derivedParameters` containing the parameter values modified
  indirectly by the algorithm. Those parameters are typically formula
  parameters.

- `seed` containing the seed value used to generate random values
