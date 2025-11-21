# Validate arguments provided as vectors

Validate arguments provided as vectors

## Usage

``` r
.cleanVectorArgs(arg = NULL, expectedLength = NULL, type)
```

## Arguments

- arg:

  A vector of arguments.

- expectedLength:

  An integer to denote the expected length of the vector.

- type:

  A single string or a vector of string representation or class of the
  type that should be checked for.

## Value

An atomic vector of desired data type.

## Details

Cleaning an argument provided as (atomic or generic) vector involves:

- Checking that it is of expected length.

- Checking for `NULL` or other special constants (`NaN`, `Inf`, `NA` of
  the wrong type) and standardizing them to `NA` of desired data type.

- Checking that each element in the vector is of expected data type.

- Making sure that an atomic vector is always returned, irrespective of
  if the input was a list or an atomic vector.

## Examples

``` r
ospsuite:::.cleanVectorArgs(list(1, 2, NA, NULL), 4L, "numeric")
#> [1]  1  2 NA NA
```
