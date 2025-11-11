# Get dimension by name

Get dimension by name

## Usage

``` r
getDimensionByName(name)
```

## Arguments

- name:

  Name of dimension that should be retrieved

## Value

Returns the an instance of the dimension with the given name if found or
`NULL` otherwise.

## Examples

``` r
getDimensionByName("Time")
#> 
#> ── <NetObject> ──
#> 
#> Type: OSPSuite.Core.Domain.UnitSystem.Dimension
#> 
#> ── Available Methods 
#>   • `AddUnit()`
#>   • `AddUnit()`
#>   • `AddUnit()`
#>   • `BaseUnitValueToUnitValue()`
#>   • `BaseUnitValueToUnitValue()`
#>   • `CanConvertToUnit()`
#>   • `CompareTo()`
#>   • `CompareTo()`
#>   • `Equals()`
#>   • `FindUnit()`
#>   • `get_BaseRepresentation()`
#>   • `get_BaseUnit()`
#>   • `get_DefaultUnit()`
#>   • `get_DefaultUnitName()`
#>   • `get_DisplayName()`
#>   • `get_Name()`
#>   • `get_Units()`
#>   • `GetHashCode()`
#>   • `GetType()`
#>   • `GetUnitNames()`
#>   • `HasUnit()`
#>   • `HasUnit()`
#>   • `RemoveUnit()`
#>   • `set_DefaultUnit()`
#>   • `set_DisplayName()`
#>   • `SupportsUnit()`
#>   • `ToString()`
#>   • `Unit()`
#>   • `UnitAt()`
#>   • `UnitOrDefault()`
#>   • `UnitValueToBaseUnitValue()`
#>   • `UnitValueToBaseUnitValue()`
#> 
#> ── Available Properties 
#>   • BaseRepresentation
#>   • BaseUnit
#>   • DefaultUnit
#>   • DefaultUnitName
#>   • DisplayName
#>   • Name
#>   • Units
```
