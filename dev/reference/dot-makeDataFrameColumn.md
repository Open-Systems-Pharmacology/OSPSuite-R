# Create a vector of the right length for a certain property of a `DataSet`

Create a vector of the right length for a certain property of a
`DataSet`

## Usage

``` r
.makeDataFrameColumn(dataSets, property, metaDataName = NULL)
```

## Arguments

- dataSets:

  A list of `dataSet` objects or a single `dataSet`.

- property:

  The property to create the vector for.

- metaDataName:

  The name of the metaData to create the vector for.

## Value

A vector of length corresponding to dataSet\$xValues containing the
property values.
