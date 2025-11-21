# PopulationCharacteristics

Characteristics of a population used for population creation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `PopulationCharacteristics`

## Active bindings

- `numberOfIndividuals`:

  Number of individuals in the population

- `proportionOfFemales`:

  Proportion of female in the population

- `species`:

  Specifies the species of the individual. It should be a species
  available in PK-Sim (see `Species`)

- `population`:

  For a Human species, the population of interest. It should be a
  population available in PK-Sim (see `HumanPopulation`)

- `age`:

  Age range of the population as in instance of a `ParameterRange`
  (optional)

- `gestationalAge`:

  Gestational Age range of the population as in instance of a
  `ParameterRange` (optional)

- `weight`:

  Weight range of the population as in instance of a `ParameterRange`
  (optional)

- `height`:

  Height range of the population as in instance of a `ParameterRange`
  (optional)

- `BMI`:

  BMI range of the population as in instance of a `ParameterRange`
  (optional)

- `allMoleculeOntogenies`:

  All molecule ontogenies defined for this population characteristics.

- `seed`:

  Seed used to generate the population

## Methods

### Public methods

- [`PopulationCharacteristics$new()`](#method-PopulationCharacteristics-new)

- [`PopulationCharacteristics$print()`](#method-PopulationCharacteristics-print)

- [`PopulationCharacteristics$addMoleculeOntogeny()`](#method-PopulationCharacteristics-addMoleculeOntogeny)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/rSharp/html/NetObject.html#method-NetObject-set)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class

#### Usage

    PopulationCharacteristics$new()

#### Returns

A new `PopulationCharacteristics` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    PopulationCharacteristics$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `addMoleculeOntogeny()`

Add a molecule ontogeny `MoleculeOntogeny` to the individual
characteristics

#### Usage

    PopulationCharacteristics$addMoleculeOntogeny(moleculeOntogeny)

#### Arguments

- `moleculeOntogeny`:

  Molecule ontogeny to add
