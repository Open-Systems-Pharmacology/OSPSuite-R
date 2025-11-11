# Replace missing groupings with dataset names

Datasets which haven't been assigned to any group will be plotted as a
group on its own. That is, the `group` column entries for them will be
their names.

## Usage

``` r
.addMissingGroupings(data)
```

## Arguments

- data:

  A data frame returned by `DataCombined$toDataFrame()`.

## See also

Other utilities-plotting:
[`.convertGeneralToSpecificPlotConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-convertGeneralToSpecificPlotConfiguration.md),
[`.createAxesLabels()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-createAxesLabels.md),
[`.extractAggregatedSimulatedData()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/dot-extractAggregatedSimulatedData.md)

## Examples

``` r
df <- dplyr::tibble(
  group = c(
    "Stevens 2012 solid total",
    "Stevens 2012 solid total",
    NA,
    NA,
    NA
  ),
  name = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Sita_dist",
    "Stevens_2012_placebo.Sita_proximal",
    "Stevens_2012_placebo.Sita_total"
  ),
  dataType = c(
    "simulated",
    "observed",
    "observed",
    "observed",
    "observed"
  )
)

# original
df
#> # A tibble: 5 × 3
#>   group                    name                                         dataType
#>   <chr>                    <chr>                                        <chr>   
#> 1 Stevens 2012 solid total Organism|Lumen|Stomach|Metformin|Gastric re… simulat…
#> 2 Stevens 2012 solid total Stevens_2012_placebo.Placebo_total           observed
#> 3 NA                       Stevens_2012_placebo.Sita_dist               observed
#> 4 NA                       Stevens_2012_placebo.Sita_proximal           observed
#> 5 NA                       Stevens_2012_placebo.Sita_total              observed

# transformed
ospsuite:::.addMissingGroupings(df)
#> # A tibble: 5 × 3
#>   group                              name                               dataType
#>   <chr>                              <chr>                              <chr>   
#> 1 Stevens 2012 solid total           Organism|Lumen|Stomach|Metformin|… simulat…
#> 2 Stevens 2012 solid total           Stevens_2012_placebo.Placebo_total observed
#> 3 Stevens_2012_placebo.Sita_dist     Stevens_2012_placebo.Sita_dist     observed
#> 4 Stevens_2012_placebo.Sita_proximal Stevens_2012_placebo.Sita_proximal observed
#> 5 Stevens_2012_placebo.Sita_total    Stevens_2012_placebo.Sita_total    observed
```
