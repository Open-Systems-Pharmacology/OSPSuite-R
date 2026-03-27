# Creates a list of `DataSet` objects from a `data.frame`

Creates a list of `DataSet` objects from a `data.frame`

## Usage

``` r
dataSetsFromDataFrame(data)
```

## Arguments

- data:

  A `data.frame` with at minimum the columns `name`, `xValues`, and
  `yValues`. Optional standard columns: `yErrorValues`, `xDimension`,
  `xUnit`, `yDimension`, `yUnit`, `yErrorType`, `yErrorUnit`,
  `molWeight`, `lloq`. Any additional columns are treated as meta data
  entries.

## Value

A named list of `DataSet` objects, named by the `name` column.

## Details

Creates `DataSet` objects from a `data.frame` with the same structure as
returned by
[`dataSetToDataFrame()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dataSetToDataFrame.md).
Each unique value in the `name` column results in one `DataSet` object.
Any columns beyond the standard columns (`name`, `xValues`, `yValues`,
`yErrorValues`, `xDimension`, `xUnit`, `yDimension`, `yUnit`,
`yErrorType`, `yErrorUnit`, `molWeight`, `lloq`) will be added as meta
data.

## Examples

``` r
dataSet <- DataSet$new(name = "MyData")
dataSet$setValues(xValues = c(1, 2, 3), yValues = c(10, 20, 30))
df <- dataSetToDataFrame(dataSet)
dataSets <- dataSetsFromDataFrame(df)
```
