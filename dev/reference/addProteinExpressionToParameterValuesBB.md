# Extend a Parameter Values Building Block (BB) with protein expression parameters for selected protein molecules in the selected organs.

TBD: is a spatial structure required? Option 1 - define the organs as
paths Option 2 - provide a spatial structure and then define organ
paths. Entries will be created for all sub-organs.

## Usage

``` r
addProteinExpressionToParameterValuesBB(
  parameterValuesBuildingBlock,
  spatialStructureBB,
  organPaths = NULL,
  moleculesBB,
  moleculeNames = NULL
)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`.

- spatialStructureBB:

  A `BuildingBlock` object of type `Spatial Structure`. Entries will be
  created for the selected molecules in all physical containers of this
  spatial structure.

- organPaths:

  A vector of paths to the organs for which the expression paramters
  will be created. If any of the provided path is not an organ, an error
  is thrown. If `NULL` (default), the function will use all organs from
  the spatial structure.

- moleculesBB:

  A `BuildingBlock` object of type `Molecules`. The entries will be
  created for all proteins from this building block, or for a subset of
  protein molecules defined in the `moleculeNames` argument.

- moleculeNames:

  Optional list of protein molecule names. If provided, only the
  molecules with these names will be added to the
  `parameterValuesBuildingBlock`.

## Value

Path of entries added to the building block.

## Details

TBD: Is a molecules BB required? Or just molecule names?
