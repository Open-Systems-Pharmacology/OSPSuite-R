# MoleculesBuildingBlock

A `Molecules` building block. Subclass of
[BuildingBlock](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.md).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\>
[`BuildingBlock`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.md)
-\> `MoleculesBuildingBlock`

## Methods

### Public methods

- [`MoleculesBuildingBlock$new()`](#method-MoleculesBuildingBlock-initialize)

- [`MoleculesBuildingBlock$allMoleculeNames()`](#method-MoleculesBuildingBlock-allMoleculeNames)

- [`MoleculesBuildingBlock$allFloatingMoleculeNames()`](#method-MoleculesBuildingBlock-allFloatingMoleculeNames)

- [`MoleculesBuildingBlock$allStationaryMoleculeNames()`](#method-MoleculesBuildingBlock-allStationaryMoleculeNames)

- [`MoleculesBuildingBlock$allMoleculeNamesOfType()`](#method-MoleculesBuildingBlock-allMoleculeNamesOfType)

- [`MoleculesBuildingBlock$allXenobioticFloatingMoleculeNames()`](#method-MoleculesBuildingBlock-allXenobioticFloatingMoleculeNames)

- [`MoleculesBuildingBlock$allEndogenousStationaryMoleculeNames()`](#method-MoleculesBuildingBlock-allEndogenousStationaryMoleculeNames)

- [`MoleculesBuildingBlock$moleculeTypeFor()`](#method-MoleculesBuildingBlock-moleculeTypeFor)

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
- [`BuildingBlock$print()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.html#method-print)

------------------------------------------------------------------------

### `MoleculesBuildingBlock$new()`

Initialize a new instance of the class.

#### Usage

    MoleculesBuildingBlock$new(netObject)

#### Arguments

- `netObject`:

  Reference to the underlying `Molecules` building block.

#### Returns

A new `MoleculesBuildingBlock` object.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$allMoleculeNames()`

Returns the names of all molecules defined in the building block.

#### Usage

    MoleculesBuildingBlock$allMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$allFloatingMoleculeNames()`

Returns the names of all floating molecules in the building block
(molecules with `IsFloating = TRUE`, e.g. drugs, metabolites).

#### Usage

    MoleculesBuildingBlock$allFloatingMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$allStationaryMoleculeNames()`

Returns the names of all stationary molecules in the building block
(molecules with `IsFloating = FALSE`, e.g. enzymes, transporters).

#### Usage

    MoleculesBuildingBlock$allStationaryMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$allMoleculeNamesOfType()`

Returns the names of all molecules of the given type. Pass
`MoleculeType$Protein` to obtain all proteins (the union of `Enzyme`,
`Transporter`, and `Binding Partner`).

#### Usage

    MoleculesBuildingBlock$allMoleculeNamesOfType(moleculeType)

#### Arguments

- `moleculeType`:

  One of the values defined in the `MoleculeType` enum (e.g. `Drug`,
  `Metabolite`, `Enzyme`, `Transporter`, `Binding Partner`, `Complex`,
  `Protein`).

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$allXenobioticFloatingMoleculeNames()`

Returns the names of all xenobiotic floating molecules in the building
block (e.g. drugs, drug complexes).

#### Usage

    MoleculesBuildingBlock$allXenobioticFloatingMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$allEndogenousStationaryMoleculeNames()`

Returns the names of all endogenous stationary molecules in the building
block (e.g. enzymes, transporters, other proteins).

#### Usage

    MoleculesBuildingBlock$allEndogenousStationaryMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### `MoleculesBuildingBlock$moleculeTypeFor()`

Returns the type of a molecule by name as it is recorded in the building
block (e.g. `"Drug"`, `"Enzyme"`, `"Transporter"`, `"Binding Partner"`).
Throws an error if the molecule is not present.

#### Usage

    MoleculesBuildingBlock$moleculeTypeFor(moleculeName)

#### Arguments

- `moleculeName`:

  Name of the molecule to look up.

#### Returns

Character string with the molecule type.
