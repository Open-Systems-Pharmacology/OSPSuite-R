# Creates an individual using the PK-Sim Database.

Creates an individual using the PK-Sim Database.

## Usage

``` r
createIndividualCharacteristics(
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
  seed = NULL
)
```

## Arguments

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

- moleculeOntogenies:

  Optional list of `MoleculeOntogeny` that will be used to retrieve
  ontogeny information for molecules.

- seed:

  Optional seed parameter to use to generate start values for the
  created individual algorithm. A `MoleculeOntogeny` is an object with
  the name a `molecule` property (e.g the name of the molecule as
  defined in your simulation) and an `ontogeny` property (e.g. the name
  of the predefined ontogeny to use for this molecule). The list of all
  available ontogenies can be accessed programmatically using the enum
  `StandardOntogeny`

## Value

An array of `ParameterValue` containing the value of each individual
parameter
