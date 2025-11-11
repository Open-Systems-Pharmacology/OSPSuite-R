# Retrieves the name of the constant in the specified enumeration that has the specified value.

Retrieves the name of the constant in the specified enumeration that has
the specified value.

## Usage

``` r
.netEnumName(enumType, enumValue)
```

## Arguments

- enumType:

  a .NET object, System.Type or type name, possibly namespace and
  assembly qualified type name, e.g.
  'My.Namespace.MyClass,MyAssemblyName'.

- enumValue:

  The value of a particular enumerated constant in terms of its
  underlying type. Typically an integer.

## Value

A string containing the name of the enumerated constant in `enumType`
whose value is `enumValue`; or `null` if no such constant is found.

## Examples

``` r
ospsuite:::.netEnumName("OSPSuite.Core.Domain.Data.AuxiliaryType", 2L)
#> [1] "GeometricStdDev"
```
