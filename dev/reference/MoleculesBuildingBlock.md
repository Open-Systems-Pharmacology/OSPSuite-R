# MoleculesBuildingBlock

A `Molecules` building block. Subclass of
[BuildingBlock](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.md).

## Super classes

[`rSharp::NetObject`](http://www.open-systems-pharmacology.org/rSharp/reference/NetObject.md)
-\>
[`ospsuite::DotNetWrapper`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/DotNetWrapper.md)
-\>
[`ospsuite::ObjectBase`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ObjectBase.md)
-\>
[`ospsuite::BuildingBlock`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/BuildingBlock.md)
-\> `MoleculesBuildingBlock`

## Methods

### Public methods

- [`MoleculesBuildingBlock$new()`](#method-MoleculesBuildingBlock-new)

- [`MoleculesBuildingBlock$allMoleculeNames()`](#method-MoleculesBuildingBlock-allMoleculeNames)

- [`MoleculesBuildingBlock$allFloatingMoleculeNames()`](#method-MoleculesBuildingBlock-allFloatingMoleculeNames)

- [`MoleculesBuildingBlock$allStationaryMoleculeNames()`](#method-MoleculesBuildingBlock-allStationaryMoleculeNames)

- [`MoleculesBuildingBlock$allMoleculeNamesOfType()`](#method-MoleculesBuildingBlock-allMoleculeNamesOfType)

- [`MoleculesBuildingBlock$allXenobioticFloatingMoleculeNames()`](#method-MoleculesBuildingBlock-allXenobioticFloatingMoleculeNames)

- [`MoleculesBuildingBlock$allEndogenousStationaryMoleculeNames()`](#method-MoleculesBuildingBlock-allEndogenousStationaryMoleculeNames)

- [`MoleculesBuildingBlock$moleculeTypeFor()`](#method-MoleculesBuildingBlock-moleculeTypeFor)

Inherited methods

- [`rSharp::NetObject$.printClass()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-.printClass)
- [`rSharp::NetObject$.printLine()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-.printLine)
- [`rSharp::NetObject$call()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-call)
- [`rSharp::NetObject$get()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-get)
- [`rSharp::NetObject$getFields()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getFields)
- [`rSharp::NetObject$getMemberSignature()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getMemberSignature)
- [`rSharp::NetObject$getMethods()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getMethods)
- [`rSharp::NetObject$getProperties()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getProperties)
- [`rSharp::NetObject$getStaticFields()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticFields)
- [`rSharp::NetObject$getStaticMethods()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticMethods)
- [`rSharp::NetObject$getStaticProperties()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-getStaticProperties)
- [`rSharp::NetObject$set()`](https://www.open-systems-pharmacology.org/OSPSuite-R/rSharp/html/NetObject.html#method-NetObject-set)
- [`ospsuite::BuildingBlock$print()`](https://www.open-systems-pharmacology.org/OSPSuite-R/ospsuite/html/BuildingBlock.html#method-BuildingBlock-print)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class.

#### Usage

    MoleculesBuildingBlock$new(netObject)

#### Arguments

- `netObject`:

  Reference to the underlying `Molecules` building block.

#### Returns

A new `MoleculesBuildingBlock` object.

------------------------------------------------------------------------

### Method `allMoleculeNames()`

Returns the names of all molecules defined in the building block.

#### Usage

    MoleculesBuildingBlock$allMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### Method `allFloatingMoleculeNames()`

Returns the names of all floating molecules in the building block
(molecules with `IsFloating = TRUE`, e.g. drugs, metabolites).

#### Usage

    MoleculesBuildingBlock$allFloatingMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### Method `allStationaryMoleculeNames()`

Returns the names of all stationary molecules in the building block
(molecules with `IsFloating = FALSE`, e.g. enzymes, transporters).

#### Usage

    MoleculesBuildingBlock$allStationaryMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### Method `allMoleculeNamesOfType()`

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

### Method `allXenobioticFloatingMoleculeNames()`

Returns the names of all xenobiotic floating molecules in the building
block (e.g. drugs, drug complexes).

#### Usage

    MoleculesBuildingBlock$allXenobioticFloatingMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### Method `allEndogenousStationaryMoleculeNames()`

Returns the names of all endogenous stationary molecules in the building
block (e.g. enzymes, transporters, other proteins).

#### Usage

    MoleculesBuildingBlock$allEndogenousStationaryMoleculeNames()

#### Returns

Character vector of molecule names.

------------------------------------------------------------------------

### Method `moleculeTypeFor()`

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
