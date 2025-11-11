# Get the parent container of a specific type for a given Entity

Recursively retrieves the parent container of the specified type for a
given `Entity`.

## Usage

``` r
.getParentContainerByType(entity, type)
```

## Arguments

- entity:

  An `Entity` object or an object inheriting from `Entity`

- type:

  A string representing the container type to find (e.g., "Simulation",
  "Molecule").

## Value

The closest parent container of the specified type or `NA` if not found.
