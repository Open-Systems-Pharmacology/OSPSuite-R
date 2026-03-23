# Extend a Parameter Values Building Block (BB) with local molecule parameters for molecules from a molecules BB in all physical containers of a spatial structure BB.

Existing entries will not be overwritten.

## Usage

``` r
addLocalMoleculeParameters(
  parameterValuesBuildingBlock,
  spatialStructureBB,
  moleculesBB,
  moleculeNames = NULL
)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`.

- spatialStructureBB:

  A `BuildingBlock` object of type `Spatial Structure`. Entries will be
  created for local parameters of the selected molecules in all physical
  containers of this spatial structure.

- moleculesBB:

  A `BuildingBlock` object of type `Molecules`. The entries will be
  created for all molecules from this building block, or for a subset of
  molecules defined in the `moleculeNames` argument.

- moleculeNames:

  Optional list of molecule names. If provided, only the molecules with
  these names will be added to the `parameterValuesBuildingBlock`.

## Value

Path of entries added to the building block.
