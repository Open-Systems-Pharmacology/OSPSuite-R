# Create an Individual Building Block

Create an Individual Building Block

## Usage

``` r
createIndividualBuildingBlock(
  species,
  population = NULL,
  gender = NULL,
  weight = NULL,
  weightUnit = "kg",
  height = NULL,
  heightUnit = "cm",
  age = NULL,
  ageUnit = "year(s)",
  gestationalAge = 40,
  gestationalAgeUnit = "week(s)",
  moleculeOntogenies = NULL,
  seed = NULL,
  diseaseState = IndividualDiseaseStates$None
)
```

## Arguments

- seed:

## Value

An object of type `BuildingBlock` representing an individual
