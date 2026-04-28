# Create a MoBi Individual Building Block

Creates an individual building block in MoBi for a given species and
optional demographic characteristics. Currently, disease states are not
supported.

## Usage

``` r
createIndividualBuildingBlock(
  name = NULL,
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
  seed = NULL
)
```

## Arguments

- name:

  Optional name for the individual building block. If not provided,
  species will be used as name.

- species:

  Species of the individual as defined in PK-Sim (see Species enum)

- population:

  Population to use to create the individual. This is required only when
  the species is Human. (See HumanPopulation enum)

- gender:

  Gender to use to create the individual. (See Gender enum)

- weight:

  Weight of the created individual

- weightUnit:

  Unit in which the weight value is defined. Default is kg

- height:

  Height of the created individual (for human species only)

- heightUnit:

  Unit in which the height value is defined. Default is cm

- age:

  Age of the created individual (for human species only)

- ageUnit:

  Unit in which the age value is defined. Default is year(s)

- gestationalAge:

  Gestational age of the created individual (for human species only
  using the Preterm population). Default is 40 Weeks

- gestationalAgeUnit:

  Unit in which the gestational age value is defined. Default is week(s)

- seed:

  Optional seed parameter to use to generate start values for the
  created individual algorithm.

## Value

An object of type `BuildingBlock` representing the created individual.

## Examples

``` r
individual <- createIndividualBuildingBlock(
  name = "Standard Individual",
  species = Species$Human,
  population = HumanPopulation$European_ICRP_2002,
  gender = Gender$Male,
  weight = 73,
  age = 30
)
```
