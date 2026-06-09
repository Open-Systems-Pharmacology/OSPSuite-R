# MoleculeOntogeny

Use when retrieving individual values using the
createIndividualAlgorithm. This class is a simple pair (MoleculeName,
Ontogeny) allowing the user to retrieve potential ontogeny values.

## Public fields

- `molecule`:

  Name of the molecule in the model

- `ontogeny`:

  Name of the ontogeny to use for the molecule

## Methods

### Public methods

- [`MoleculeOntogeny$new()`](#method-MoleculeOntogeny-initialize)

- [`MoleculeOntogeny$print()`](#method-MoleculeOntogeny-print)

- [`MoleculeOntogeny$printMoleculeOntogeny()`](#method-MoleculeOntogeny-printMoleculeOntogeny)

------------------------------------------------------------------------

### `MoleculeOntogeny$new()`

Initialize a new instance of the class

#### Usage

    MoleculeOntogeny$new(molecule, ontogeny)

#### Arguments

- `molecule`:

  molecule name

- `ontogeny`:

  ontogeny to use for the Molecule (one of StandardOntogeny)

#### Returns

A new `MoleculeOntogeny` object.

------------------------------------------------------------------------

### `MoleculeOntogeny$print()`

Print the object to the console

#### Usage

    MoleculeOntogeny$print(...)

#### Arguments

- `...`:

  Rest arguments.

------------------------------------------------------------------------

### `MoleculeOntogeny$printMoleculeOntogeny()`

Print the `MoleculeOntogeny` on one line

#### Usage

    MoleculeOntogeny$printMoleculeOntogeny()
