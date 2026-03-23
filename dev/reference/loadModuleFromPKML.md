# Load a MoBi module from pkml.

Load a MoBi module from pkml.

## Usage

``` r
loadModuleFromPKML(path)
```

## Arguments

- path:

  Path to the pkml file with a module export

## Value

A `MoBiModule` object

## Examples

``` r
filePath <- system.file("extdata", "Thyroid.pkml", package = "ospsuite")
module <- loadModuleFromPKML(filePath)
```
