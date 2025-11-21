# Returns an instance of a PK-Parameter by name or NULL if the parameter by name is not found

Returns an instance of a PK-Parameter by name or NULL if the parameter
by name is not found

## Usage

``` r
pkParameterByName(name, stopIfNotFound = TRUE)
```

## Arguments

- name:

  Name of PK-Parameter to update

- stopIfNotFound:

  Boolean. If `TRUE` (default) and no pk parameter exist for the given
  name, an error is thrown. If `FALSE`, `NULL` is returned.

## Examples

``` r
pkParameter <- pkParameterByName(name = "t_max")
```
