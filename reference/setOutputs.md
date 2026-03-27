# Set outputs of a simulation

Sets the quantities as output of the `simulation`. The quantities can
either be specified using explicit instances or using paths. This
function clears the output selection before adding the new quantities.
See `addOutputs` for adding quantities without clearing the output
selection. See `clearOutputs` for clearing the output selection without
adding new quantities.

## Usage

``` r
setOutputs(quantitiesOrPaths, simulation, stopIfNotFound = TRUE)
```

## Arguments

- quantitiesOrPaths:

  Quantity instances (element or vector) (typically retrieved using
  `getAllQuantitiesMatching`) or quantity path (element or vector) to
  add.

- simulation:

  Instance of a simulation for which output selection should be updated.

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no quantity exists for the given
  path, an error is thrown. If `FALSE`, `NULL` is returned.
