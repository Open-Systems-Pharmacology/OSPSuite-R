# Set molecule scale divisors

Set molecule scale divisors

## Usage

``` r
setMoleculeScaleDivisors(molecules, values)
```

## Arguments

- molecules:

  A single or a list of `Molecule`

- values:

  A numeric value that should be assigned to the molecule scale factor
  or a vector of numeric values, if the scale factor of more than one
  molecule should be changed. Must have the same length as `molecules`

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
setMoleculeScaleDivisors(molecule, 0.001)
molecules <- getAllMoleculesMatching("Organism|**|A", sim)
setMoleculeScaleDivisors(molecules, c(0.002, 0.003))
```
