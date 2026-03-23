# Extend an Initial Conditions Building Block (BB) with new entries for molecules from a molecules BB in all physical containers of a spatial structure BB.

If an initial condition for the combination container, molecule already
exists in the IC-BB, the value will be kept AS IS.

## Usage

``` r
extendInitialConditions(
  initialConditionsBuildingBlock,
  spatialStructureBB,
  moleculesBB,
  moleculeNames = NULL
)
```

## Arguments

- initialConditionsBuildingBlock:

  A `BuildingBlock` object of type `Initial Conditions`.

- spatialStructureBB:

  A `BuildingBlock` object of type `Spatial Structure`. Entries will be
  created for the selected molecules in all physical containers of this
  spatial structure.

- moleculesBB:

  A `BuildingBlock` object of type `Molecules`. The entries will be
  created for all molecules from this building block, or for a subset of
  molecules defined in the `moleculeNames` argument.

- moleculeNames:

  Optional list of molecule names. If provided, only the molecules with
  these names will be added to the `initialConditionsBuildingBlock`.

## Value

Paths of entries added to the building block.
