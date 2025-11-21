# Encoding mu symbol

Encoding mu symbol

## Usage

``` r
.encodeUnit(unit)
```

## Arguments

- unit:

  Unit to encode.

## Details

This is required to ensure that we have no issue using the mu symbol in
different OS See
https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/476 for
details.

## Examples

``` r
ospsuite:::.encodeUnit("µl")
#> [1] "µl"
```
