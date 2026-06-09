# PopulationCharacteristics

Characteristics of a population used for population creation

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
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

- [`PopulationCharacteristics$new()`](#method-PopulationCharacteristics-initialize)

- [`PopulationCharacteristics$print()`](#method-PopulationCharacteristics-print)

- [`PopulationCharacteristics$addMoleculeOntogeny()`](#method-PopulationCharacteristics-addMoleculeOntogeny)

Inherited methods

- [`rSharp::NetObject$.printClass()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printClass)
- [`rSharp::NetObject$.printLine()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-.printLine)
- [`rSharp::NetObject$call()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-call)
- [`rSharp::NetObject$get()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-get)
- [`rSharp::NetObject$getFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getFields)
- [`rSharp::NetObject$getMemberSignature()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getMethods)
- [`rSharp::NetObject$getProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getProperties)
- [`rSharp::NetObject$getStaticFields()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-getStaticProperties)
- [`rSharp::NetObject$set()`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.html#method-set)

------------------------------------------------------------------------

### `PopulationCharacteristics$new()`

Initialize a new instance of the class

#### Usage

    PopulationCharacteristics$new()

#### Returns

A new `PopulationCharacteristics` object.

------------------------------------------------------------------------

### `PopulationCharacteristics$print()`

Print the object to the console

#### Usage

    PopulationCharacteristics$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### `PopulationCharacteristics$addMoleculeOntogeny()`

Add a molecule ontogeny `MoleculeOntogeny` to the individual
characteristics

#### Usage

    PopulationCharacteristics$addMoleculeOntogeny(moleculeOntogeny)

#### Arguments

- `moleculeOntogeny`:

  Molecule ontogeny to add
