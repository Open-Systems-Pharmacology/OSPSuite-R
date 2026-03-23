# Validate that an object is a named list

Validate that an object is a named list

## Usage

``` r
validateIsNamedList(x, varName)
```

## Arguments

- x:

  Object to validate as a named list.

- varName:

  Name of the variable being validated (used in error message).

## Value

invisible TRUE if the validation passed, otherwise an error is thrown.

## Examples

``` r
validateIsNamedList(list(a = 1, b = 2), "myVar") # passes
# validateIsNamedList(list(1, 2), "myVar") # throws an error
```
