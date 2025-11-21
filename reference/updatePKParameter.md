# Updates some properties of a PK-Parameter (displayName and displayUnit)

Updates some properties of a PK-Parameter (displayName and displayUnit)

## Usage

``` r
updatePKParameter(name, displayName = NULL, displayUnit = NULL)
```

## Arguments

- name:

  Name of PK-Parameter to update

- displayName:

  Optional display name

- displayUnit:

  Optional display unit. Note that the unit should be defined in unit of
  the dimension

## Examples

``` r
updatePKParameter("t_max", "MyTmax", "min")
#> <PKParameter>
#>   • Name: t_max
#>   • DisplayName: MyTmax
#>   • Dimension: Time
#>   • DisplayUnit: min
```
