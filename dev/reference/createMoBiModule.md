# Create a new MoBi module

Creates an empty MoBi module, optionally pre-populated with a list of
building blocks.

## Usage

``` r
createMoBiModule(name, buildingBlocks = NULL)
```

## Arguments

- name:

  Name of the module to create. Must be a non-empty string.

- buildingBlocks:

  Optional `BuildingBlock`, list of `BuildingBlock` objects, or `NULL`
  (default) for an empty module.

## Value

A `MoBiModule` object.

## Details

Single-type building blocks (Molecules, Reactions, Spatial Structure,
Passive Transports, Observers, Event Groups) can appear at most once per
module; passing two BBs of the same single type raises an error. Initial
Conditions and Parameter Values BBs may appear multiple times.

## Examples

``` r
if (FALSE) { # \dontrun{
# Empty module
module <- createMoBiModule("MyModule")

# Module pre-populated with a Parameter Values BB loaded from a pkml file
pvBB <- loadBuildingBlockFromPKML(
  filePath = "ParameterValues.pkml",
  type = BuildingBlockTypes$`Parameter Values`
)
module <- createMoBiModule("MyModule", buildingBlocks = pvBB)
} # }
```
