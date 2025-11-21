# Clears the memory used by all underlying objects

Clears the memory used by all underlying objects

## Usage

``` r
clearMemory(clearSimulationsCache = FALSE)
```

## Arguments

- clearSimulationsCache:

  optional - Should the simulation cache also be cleared? Default is
  `FALSE`.

## Details

The function aims at clearing the memory used by object references
allocated during some workflows. The memory should typically be freed
automatically when the system is under memory pressure or when the
garbage collection is kicking in. However, it may be necessary sometimes
to explicitly start the garbage collection process.

## Examples

``` r
# This will clear the memory and also clear the simulations cache but leave
# the environment intact.
clearMemory(clearSimulationsCache = TRUE)
```
