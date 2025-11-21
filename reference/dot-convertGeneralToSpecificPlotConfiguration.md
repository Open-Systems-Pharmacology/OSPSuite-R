# Create plot-specific `tlf::PlotConfiguration` object

Create plot-specific
[`tlf::PlotConfiguration`](https://rdrr.io/pkg/tlf/man/PlotConfiguration.html)
object

## Usage

``` r
.convertGeneralToSpecificPlotConfiguration(
  specificPlotConfiguration,
  generalPlotConfiguration
)
```

## Arguments

- specificPlotConfiguration:

  A specific subclass of
  [`tlf::PlotConfiguration`](https://rdrr.io/pkg/tlf/man/PlotConfiguration.html)
  needed for the given plot.

- generalPlotConfiguration:

  A `DefaultPlotConfiguration` object.

## Details

The default plot configuration and the labels needs to vary from
plot-to-plot because each plot has its specific (default) aesthetic
needs that need to be met.

For example, although the axes labels for profile plots will be (e.g.)
"Time vs Fraction", they will be (e.g.) "Observed vs simulated values"
for scatter plots. Additionally, mapping group to line colors might be
desirable for a profile plot, it is not so for scatter plots.

This function generates object of specific subclass of
[`tlf::PlotConfiguration`](https://rdrr.io/pkg/tlf/man/PlotConfiguration.html)
needed for the given plot but with suitable defaults taken from the
`DefaultPlotConfiguration` object.

## See also

Other utilities-plotting:
[`.addMissingGroupings()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-addMissingGroupings.md),
[`.createAxesLabels()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-createAxesLabels.md),
[`.extractAggregatedSimulatedData()`](https://www.open-systems-pharmacology.org/OSPSuite-R/reference/dot-extractAggregatedSimulatedData.md)

## Examples

``` r
ospsuite:::.convertGeneralToSpecificPlotConfiguration(
  tlf::TimeProfilePlotConfiguration$new(),
  ospsuite::DefaultPlotConfiguration$new()
)
#> <TimeProfilePlotConfiguration>
#>   Inherits from: <PlotConfiguration>
#>   Public:
#>     background: active binding
#>     clone: function (deep = FALSE) 
#>     defaultExpand: FALSE
#>     defaultSymmetricAxes: FALSE
#>     defaultXScale: lin
#>     defaultYScale: lin
#>     errorbars: active binding
#>     export: ExportConfiguration, R6
#>     initialize: function (..., y2label = NULL, y2Axis = NULL, y2Scale = NULL, 
#>     labels: active binding
#>     legend: active binding
#>     lines: active binding
#>     lloqDirection: horizontal
#>     points: active binding
#>     ribbons: active binding
#>     xAxis: active binding
#>     y2Axis: active binding
#>     yAxis: active binding
#>   Private:
#>     .background: BackgroundConfiguration, R6
#>     .errorbars: ThemeAestheticSelections, ThemeAestheticMaps, R6
#>     .labels: LabelConfiguration, R6
#>     .legend: LegendConfiguration, R6
#>     .lines: ThemeAestheticSelections, ThemeAestheticMaps, R6
#>     .points: ThemeAestheticSelections, ThemeAestheticMaps, R6
#>     .ribbons: ThemeAestheticSelections, ThemeAestheticMaps, R6
#>     .xAxis: XAxisConfiguration, AxisConfiguration, R6
#>     .y2Axis: YAxisConfiguration, AxisConfiguration, R6
#>     .yAxis: YAxisConfiguration, AxisConfiguration, R6
```
