# IndividualCharacteristics

Characteristics of an individual describing its origin

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DotNetWrapper.md)
-\> `IndividualCharacteristics`

## Active bindings

- `species`:

  Specifies the species of the individual. It should be a species
  available in PK-Sim (see `Species`)

- `population`:

  For a Human species, the population of interest. It should be a
  population available in PK-Sim (see `HumanPopulation`)

- `gender`:

  Gender of the individual. It should be defined for the species in
  PK-Sim (see `Gender`)

- `age`:

  Age of the individual as in instance of a `SnapshotParameter`
  (optional)

- `gestationalAge`:

  Gestational Age of the individual as in instance of a
  `SnapshotParameter` (optional)

- `weight`:

  Weight of the individual as in instance of a `SnapshotParameter`
  (optional)

- `height`:

  Height of the individual as in instance of a `SnapshotParameter`
  (optional)

- `allMoleculeOntogenies`:

  All molecule ontogenies defined for this individual characteristics.

- `seed`:

  Seed used to generate the population

## Methods

### Public methods

- [`IndividualCharacteristics$new()`](#method-IndividualCharacteristics-new)

- [`IndividualCharacteristics$print()`](#method-IndividualCharacteristics-print)

- [`IndividualCharacteristics$addMoleculeOntogeny()`](#method-IndividualCharacteristics-addMoleculeOntogeny)

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

    IndividualCharacteristics$new()

#### Returns

A new `IndividualCharacteristics` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object to the console

#### Usage

    IndividualCharacteristics$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### Method `addMoleculeOntogeny()`

Add a molecule ontogeny `MoleculeOntogeny` to the individual
characteristics

#### Usage

    IndividualCharacteristics$addMoleculeOntogeny(moleculeOntogeny)

#### Arguments

- `moleculeOntogeny`:

  Molecule ontogeny to add
