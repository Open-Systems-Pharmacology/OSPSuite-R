# Load a building block from a pkml file

Load a building block from a pkml file

## Usage

``` r
loadBuildingBlockFromPKML(filePath, type = NULL)
```

## Arguments

- filePath:

  Path of the `pkml` file containing a single building block.

- type:

  Optional building block type to load. Must be one of the values
  defined in `BuildingBlockTypes`. If `NULL` (default), the type is
  determined automatically. When the type cannot be auto-detected, an
  error is thrown asking the caller to specify `type` explicitly.

## Value

A `BuildingBlock` object, or a specialized subclass when applicable: a
`MoleculesBuildingBlock` for `Molecules`, or an
`IndividualBuildingBlock` for `Individual`.

## Details

If `type` is supplied, an error is thrown when the file does not contain
a single building block of that type. The pkml must contain exactly one
building block; files with multiple building blocks (such as a full
simulation export) are not supported.

Supplying `type` is faster than auto-detect: with `type = NULL`, each
candidate type is tried in turn until one succeeds, which adds overhead
for every type tried before the match. Pass `type` explicitly when
known.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load with explicit type
bb <- loadBuildingBlockFromPKML(
  filePath = "molecules.pkml",
  type = BuildingBlockTypes$Molecules
)

# Auto-detect the type
bb <- loadBuildingBlockFromPKML("molecules.pkml")
} # }
```
