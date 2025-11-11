# Creates an individual using the PK-Sim Database

Creates an individual using the PK-Sim Database

## Usage

``` r
createIndividual(individualCharacteristics)
```

## Arguments

- individualCharacteristics:

  Characteristics of the individual to create as an instance of
  `IndividualCharacteristics`

## Value

A list with three entries:

- `distributedParameters` containing the actual parameter values
  modified by the create individual algorithm.

- `derivedParameters` containing the parameter values modified
  indirectly by the algorithm. Those parameters are typically formula
  parameters.

- `seed` containing the seed value used to generate random values

## Note

When updating a simulation with the value for a new individual, only use
the `distributedParameters` to ensure that you do not override formula
parameters.
