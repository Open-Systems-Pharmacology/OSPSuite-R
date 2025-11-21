# Creates the population characteristics used to create a population

Creates the population characteristics used to create a population

## Usage

``` r
createPopulationCharacteristics(
  species,
  population = NULL,
  numberOfIndividuals,
  proportionOfFemales = 50,
  weightMin = NULL,
  weightMax = NULL,
  weightUnit = "kg",
  heightMin = NULL,
  heightMax = NULL,
  heightUnit = "cm",
  ageMin = NULL,
  ageMax = NULL,
  ageUnit = "year(s)",
  BMIMin = NULL,
  BMIMax = NULL,
  BMIUnit = "kg/m²",
  gestationalAgeMin = NULL,
  gestationalAgeMax = NULL,
  gestationalAgeUnit = "week(s)",
  moleculeOntogenies = NULL,
  seed = NULL
)
```

## Arguments

- species:

  Species of the individual as defined in PK-Sim (see `Species` enum)

- population:

  Population to use to create the individual. This is required only when
  the species is Human. (See `HumanPopulation` enum)

- numberOfIndividuals:

  Number of individuals in the population

- proportionOfFemales:

  Proportions of females. Default is 50 (50%)

- weightMin:

  min weight for the population (optional)

- weightMax:

  max weight for the population (optional)

- weightUnit:

  Unit in which the weight value is defined. Default is kg

- heightMin:

  min height for the population (optional, for human species only)

- heightMax:

  max height for the population (optional, for human species only)

- heightUnit:

  Unit in which the height value is defined. Default is cm

- ageMin:

  min age for the population (optional, for human species only)

- ageMax:

  max age for the population (optional, for human species only)

- ageUnit:

  Unit in which the age value is defined. Default is year(s)

- BMIMin:

  min BMI for the population (optional, for human species only)

- BMIMax:

  max BMI for the population (optional, for human species only)

- BMIUnit:

  Unit in which the BMI value is defined. Default is kg/m2

- gestationalAgeMin:

  min gestational age for the population (optional, for human species
  only)

- gestationalAgeMax:

  max gestational age for the population (optional, for human species
  only)

- gestationalAgeUnit:

  Unit in which the gestational age value is defined. Default is kg/m2

- moleculeOntogenies:

  Optional list of `MoleculeOntogeny` that will be used to retrieve
  ontogeny information for molecules.

- seed:

  Optional Seed parameter used to generate random values. This is only
  useful in order to reproduce the same population

## Value

An instance of `PopulationCharacteristics` to be used in conjunction
with `createPopulation`
