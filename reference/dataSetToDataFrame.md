# Converts a list of `DataSet` objects to a data.frame

Converts a list of `DataSet` objects to a data.frame

## Usage

``` r
dataSetToDataFrame(dataSets)

dataSetToTibble(dataSets, names = NULL)
```

## Arguments

- dataSets:

  A list of `DataSet` objects or a single `DataSet`

- names:

  Optional character vector of custom names to assign to the datasets.
  If provided, must have the same length as the number of DataSet
  objects. This allows renaming datasets, which is particularly useful
  when multiple datasets have the same original name.

## Value

DataSet objects as data.frame with columns name, xValues, yValues,
yErrorValues, xDimension, xUnit, yDimension, yUnit, yErrorType,
yErrorUnit, molWeight, lloq, and a column for each meta data that is
present in any `DataSet`.

## Examples

``` r
# Create datasets with duplicate names
ds1 <- DataSet$new(name = "Obs")
ds1$setValues(xValues = c(1, 2), yValues = c(10, 20))

ds2 <- DataSet$new(name = "Obs")
ds2$setValues(xValues = c(3, 4), yValues = c(30, 40))

# Convert to tibble with custom names
tibble_data <- dataSetToTibble(list(ds1, ds2), names = c("Study1", "Study2"))
unique(tibble_data$name) # Returns c("Study1", "Study2")
#> [1] "Study1" "Study2"
```
