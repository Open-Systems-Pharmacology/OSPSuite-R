# Created observed versus simulated paired data

Created observed versus simulated paired data

## Usage

``` r
.extractResidualsToTibble(data, scaling)
```

## Arguments

- data:

  A data frame from `DataCombined$toDataFrame()`, which has been further
  tidied using
  [`.removeUnpairableDatasets()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-removeUnpairableDatasets.md)
  and then
  [`.unitConverter()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-unitConverter.md)
  functions.
