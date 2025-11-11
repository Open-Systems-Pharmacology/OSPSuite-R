# Set molecule start values

Set molecule start values

## Usage

``` r
setMoleculeInitialValues(molecules, values, units = NULL)
```

## Arguments

- molecules:

  A single or a list of `Molecule`

- values:

  A numeric value that should be assigned to the molecule start value or
  a vector of numeric values, if the start value of more than one
  molecule should be changed. Must have the same length as `molecules`

- units:

  A string or a list of strings defining the units of the `values`. If
  `NULL` (default), values are assumed to be in base units. If not
  `NULL`, must have the same length as `quantities`.

## See also

[`getMolecule()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getMolecule.md)
and
[`getAllMoleculesMatching()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/getAllMoleculesMatching.md)
to retrieve objects of type Molecule

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)
molecule <- getMolecule("Organism|Liver|A", sim)
setMoleculeInitialValues(molecule, 1)
molecules <- getAllMoleculesMatching("Organism|**|A", sim)
setMoleculeInitialValues(molecules, c(2, 3), units = c("pmol", "mmol"))
```
