# Wrap a loaded .NET building block reference in the appropriate R6 class. `Molecules` blocks are wrapped in `MoleculesBuildingBlock` (molecule-name queries); `Individual` blocks in `IndividualBuildingBlock` (origin-data fields); all other types use the generic `BuildingBlock` wrapper.

Wrap a loaded .NET building block reference in the appropriate R6 class.
`Molecules` blocks are wrapped in `MoleculesBuildingBlock`
(molecule-name queries); `Individual` blocks in
`IndividualBuildingBlock` (origin-data fields); all other types use the
generic `BuildingBlock` wrapper.

## Usage

``` r
.wrapLoadedBB(netBB, type)
```
