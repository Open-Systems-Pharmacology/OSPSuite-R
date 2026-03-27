# Add a residual column to paired observed/predicted data

Calculates residuals between observed and predicted values and adds them
as a new column to the provided data frame or data table. The
calculation supports three scaling methods: `"log"`, `"linear"` (alias
`"lin"`), and `"ratio"`.

## Usage

``` r
addResidualColumn(
  pairedData,
  observed = "yValuesObserved",
  predicted = "yValuesSimulated",
  residuals = "residualValues",
  scaling = "log"
)
```

## Arguments

- pairedData:

  A `data.frame` or `data.table` containing paired observed and
  predicted values. The columns specified by `observed` and `predicted`
  must exist.

- observed:

  A string specifying the column name for observed values. Default is
  `"yValuesObserved"`.

- predicted:

  A string specifying the column name for predicted (simulated) values.
  Default is `"yValuesSimulated"`.

- residuals:

  A string specifying the name for the new residuals column to be added.
  Default is `"residualValues"`.

- scaling:

  A character string specifying the scaling method. One of `"log"`
  (default), `"linear"` (or `"lin"` / `"identity"`), or `"ratio"`.

## Value

The input `pairedData` with an additional column named according to the
`residuals` argument. The column carries a `"label"` attribute
describing how residuals were computed (e.g.
`"residuals\nlog(predicted) - log(observed)"`), which can be used as a
y-axis label in plots.

## Details

### Scaling Methods

- **`"linear"` / `"lin"`**: Absolute residuals as \\residual =
  predicted - observed\\.

- **`"log"`**: Log-scale residuals as \\residual = \log(predicted) -
  \log(observed)\\. Data points where the observed or predicted value is
  zero or negative produce undefined logarithms; these are set to `NaN`
  and a warning is issued reporting the number of such points.

- **`"ratio"`**: Ratio of observed to predicted as \\residual = observed
  / predicted\\. A warning is issued when predicted values are zero or
  negative.

## See also

Other data-combined:
[`DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/DataCombined.md),
[`calculateResiduals()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/calculateResiduals.md),
[`convertUnits()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/convertUnits.md)

## Examples

``` r
if (FALSE) { # \dontrun{
paired <- data.frame(
  yValuesObserved  = c(1, 2, 4),
  yValuesSimulated = c(1.1, 1.9, 3.8)
)
addResidualColumn(paired, scaling = "log")
addResidualColumn(paired, scaling = "linear")
addResidualColumn(paired, scaling = "ratio")
} # }
```
