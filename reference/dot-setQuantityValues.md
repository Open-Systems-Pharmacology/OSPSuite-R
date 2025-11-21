# Set values of quantity

Set values of quantity

## Usage

``` r
.setQuantityValues(quantities, values, units = NULL)
```

## Arguments

- quantities:

  A single or a list of `Quantity`

- values:

  A numeric value that should be assigned to the quantity or a vector of
  numeric values, if the value of more than one quantity should be
  changed. Must have the same length as 'quantities'. Alternatively, the
  value can be a unique number. In that case, the same value will be set
  in all parameters

- units:

  A string or a list of strings defining the units of the `values`. If
  `NULL` (default), values are assumed to be in base units. If not
  `NULL`, must have the same length as `quantities`.
