# Remove unpairable datasets for computing residuals

Computing residuals by definition requires that data should be in pairs,
i.e. for every simulated dataset in a given group, there should also be
a corresponding observed dataset.

To this end, current function removes the following datasets:

- Datasets which haven't been assigned to any group.

- Datasets that are not part of a pair (i.e. a simulated dataset without
  observed dataset partner, and vice versa).

## Usage

``` r
.removeUnpairableDatasets(data)
```

## Arguments

- data:

  A data frame returned by `DataCombined$toDataFrame()`.

## Examples

``` r
df <- dplyr::tribble(
  ~name, ~dataType, ~group,
  "Sim1", "Simulated", "GroupA",
  "Sim2", "Simulated", "GroupA",
  "Obs1", "Observed", "GroupB",
  "Obs2", "Observed", "GroupB",
  "Sim3", "Simulated", "GroupC",
  "Obs3", "Observed", "GroupC",
  "Sim4", "Simulated", "GroupD",
  "Obs4", "Observed", "GroupD",
  "Obs5", "Observed", "GroupD",
  "Sim5", "Simulated", "GroupE",
  "Sim6", "Simulated", "GroupE",
  "Obs7", "Observed", "GroupE",
  "Sim7", "Simulated", "GroupF",
  "Sim8", "Simulated", "GroupF",
  "Obs8", "Observed", "GroupF",
  "Obs9", "Observed", "GroupF",
  "Sim9", "Simulated", NA,
  "Obs10", "Observed", NA
)

# original
df
#> # A tibble: 18 × 3
#>    name  dataType  group 
#>    <chr> <chr>     <chr> 
#>  1 Sim1  Simulated GroupA
#>  2 Sim2  Simulated GroupA
#>  3 Obs1  Observed  GroupB
#>  4 Obs2  Observed  GroupB
#>  5 Sim3  Simulated GroupC
#>  6 Obs3  Observed  GroupC
#>  7 Sim4  Simulated GroupD
#>  8 Obs4  Observed  GroupD
#>  9 Obs5  Observed  GroupD
#> 10 Sim5  Simulated GroupE
#> 11 Sim6  Simulated GroupE
#> 12 Obs7  Observed  GroupE
#> 13 Sim7  Simulated GroupF
#> 14 Sim8  Simulated GroupF
#> 15 Obs8  Observed  GroupF
#> 16 Obs9  Observed  GroupF
#> 17 Sim9  Simulated NA    
#> 18 Obs10 Observed  NA    

# transformed
ospsuite:::.removeUnpairableDatasets(df)
#> Following datasets were specified to be grouped but not found:
#> Sim1
#> Sim2
#> Obs1
#> Obs2
#> Sim9
#> Obs10
#> 
#> # A tibble: 12 × 3
#>    name  dataType  group 
#>    <chr> <chr>     <chr> 
#>  1 Sim3  Simulated GroupC
#>  2 Obs3  Observed  GroupC
#>  3 Sim4  Simulated GroupD
#>  4 Obs4  Observed  GroupD
#>  5 Obs5  Observed  GroupD
#>  6 Sim5  Simulated GroupE
#>  7 Sim6  Simulated GroupE
#>  8 Obs7  Observed  GroupE
#>  9 Sim7  Simulated GroupF
#> 10 Sim8  Simulated GroupF
#> 11 Obs8  Observed  GroupF
#> 12 Obs9  Observed  GroupF
```
