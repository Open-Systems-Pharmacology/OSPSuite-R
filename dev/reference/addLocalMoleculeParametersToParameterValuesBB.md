# Extend a Parameter Values Building Block (BB) with local molecule parameters for molecules from a molecules module in all physical containers of a spatial structure module.

Existing entries will not be overwritten.

## Usage

``` r
addLocalMoleculeParametersToParameterValuesBB(
  parameterValuesBuildingBlock,
  spatialStructureModule,
  moleculesModule,
  moleculeNames = NULL
)
```

## Arguments

- parameterValuesBuildingBlock:

  A `BuildingBlock` object of type `Parameter Values`.

- spatialStructureModule:

  A `MoBiModule` containing a `Spatial Structure` building block.
  Entries will be created for local parameters of the selected molecules
  in all physical containers of this spatial structure.

- moleculesModule:

  A `MoBiModule` containing a `Molecules` building block. The entries
  will be created for all molecules from this building block, or for a
  subset of molecules defined in the `moleculeNames` argument.

- moleculeNames:

  Optional list of molecule names. If provided, only the molecules with
  these names will be added to the `parameterValuesBuildingBlock`. If a
  specified molecule is not present in the provided molecules BB, it
  will be ignored.

## Value

Path of entries added to the building block.
