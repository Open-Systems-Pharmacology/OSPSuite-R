# Returns a list containing all standard global parameters defined in a `simulation` for given `moleculeName`. These parameters are typically located directly under the container named after the `moleculeName`. For the list of standard parameters

Returns a list containing all standard global parameters defined in a
`simulation` for given `moleculeName`. These parameters are typically
located directly under the container named after the `moleculeName`. For
the list of standard parameters

## Usage

``` r
getStandardMoleculeParameters(moleculeName, simulation)
```

## Arguments

- moleculeName:

  Name of molecule (Enzyme, Transporter etc..) for which global
  parameters should be returned

- simulation:

  Simulation to query for molecule parameters

## Value

A list of all standard global parameters defined for `moleculeName` if
the molecule exists in the `simulation`. Otherwise an empty list is
returned

## See also

[MoleculeParameter](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/MoleculeParameter.md)

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim1 <- loadSimulation(simPath)

parameters <- getStandardMoleculeParameters("CYP3A4", sim1)
```
