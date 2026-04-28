# Extend a Parameter Values Building Block (BB) with protein expression parameters for selected protein molecules in selected organs.

For each protein in `moleculeNames`, parameters defining the protein
expression (e.g. "Relative expression", "Fraction expressed ...", and
"Initial concentration" entries) are added to the
`parameterValuesBuildingBlock` for every physical container under each
path in `organPaths`. Default values are taken from the reference
expression profile supplied for that molecule. Existing entries are not
overwritten.

## Usage

``` r
addProteinExpressionToParameterValuesBB(
  parameterValuesBuildingBlock,
  spatialStructureModule,
  moleculesModule,
  moleculeNames = NULL,
  referenceExpressionProfiles = NULL,
  organPaths = NULL
)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`. The entries will
  be added to this building block.

- spatialStructureModule:

  A `MoBiModule` containing a `Spatial Structure` building block.
  Expression parameters are created for containers defined by this
  spatial structure.

- moleculesModule:

  A `MoBiModule` containing a `Molecules` building block. Must contain
  the proteins named in `moleculeNames`.

- moleculeNames:

  Character vector of protein molecule names for which expression
  parameters should be added. If `NULL` (default), all proteins (i.e.
  molecules of type `Enzyme`, `Transporter`, or `Binding Partner`)
  defined in the Molecules building block of `moleculesModule` are used.

- referenceExpressionProfiles:

  A single `BuildingBlock` of type `Expression Profile`, a list of such
  building blocks, or `NULL`. The matching profile for each molecule in
  `moleculeNames` is identified by the profile's `MoleculeName`
  property. For every molecule in `moleculeNames` without a supplied
  profile (or when this argument is `NULL`), a default profile is
  created via
  [`createExpressionProfileBuildingBlock()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/createExpressionProfileBuildingBlock.md)
  for species `"Human"`, using the molecule's protein type to choose the
  profile category (Enzyme -\> Metabolizing Enzyme, Transporter -\>
  Transport Protein, Binding Partner -\> Protein Binding Partner).
  Profiles whose `MoleculeName` is not in `moleculeNames` are ignored.
  Duplicate profiles (two entries with the same `MoleculeName`) raise an
  error.

- organPaths:

  Character vector of organ paths under which expression parameters are
  created (e.g. `"Organism|Kidney"`). Each path must resolve to an
  existing organ in the spatial structure. The parameters are added for
  all physical containers under these paths. If `NULL` (default),
  expression parameters are created for every organ in the spatial
  structure.

## Value

Paths of entries added to the building block.
