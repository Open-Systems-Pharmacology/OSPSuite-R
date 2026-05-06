# IndividualCharacteristics

Characteristics of an individual describing its origin

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
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
