# Save the `DataSet` to pkml

Save the `DataSet` to pkml

## Usage

``` r
saveDataSetToPKML(dataSet, filePath)
```

## Arguments

- dataSet:

  The `DataSet` object

- filePath:

  Path where the pkml file will be created

## Details

Save the `DataSet` to a pkml file that can be loaded by MoBi

## Examples

``` r
if (FALSE) { # \dontrun{
dataSet <- DataSet$new(name = "NewDataSet")
dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
dataSet$saveToPKML(filePath = "../ObsData.pkml")
} # }
```
