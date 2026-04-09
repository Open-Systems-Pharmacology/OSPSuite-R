# Loads data (typically observed data) from a PKML file and creates a `DataSet` from it. The pkml files are typically exported from PK-Sim or MoBi.

Loads data (typically observed data) from a PKML file and creates a
`DataSet` from it. The pkml files are typically exported from PK-Sim or
MoBi.

## Usage

``` r
loadDataSetFromPKML(filePath)
```

## Arguments

- filePath:

  Full path of pkml file containing the observed data to load

## Examples

``` r
filePath <- system.file("extdata", "ObsDataAciclovir_1.pkml", package = "ospsuite")

obsData <- loadDataSetFromPKML(filePath)
```
