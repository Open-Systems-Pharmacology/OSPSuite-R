# Plot configuration for OSP plots

R6 configuration class defining aesthetic properties of plots that can
be created with
[`plotIndividualTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotIndividualTimeProfile.md),
[`plotPopulationTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotPopulationTimeProfile.md),
[`plotObservedVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotObservedVsSimulated.md),
and
[`plotResidualsVsTime()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsTime.md).

To interactively explore various aesthetic properties and appearance of
plots with these properties, you can use the [Shiny
app](https://www.open-systems-pharmacology.org/TLF-Library/articles/theme-maker.html)
from `{tlf}` package.

The following sections provide more details on how to customize it
further.

## Specifying aesthetic properties

Aesthetic mappings describe how groups are mapped to visual properties
(color, shape, size, etc.) of the geometries included in the plot (e.g.
point, line, ribbon, etc.).

The supported values for each property can be seen using `{tlf}` lists:

- color, fill:
  [`tlf::ColorMaps`](https://rdrr.io/pkg/tlf/man/ColorMaps.html)

- shape: [`tlf::Shapes`](https://rdrr.io/pkg/tlf/man/Shapes.html)

- legend position:
  [`tlf::LegendPositions`](https://rdrr.io/pkg/tlf/man/LegendPositions.html)

- alignments:
  [`tlf::Alignments`](https://rdrr.io/pkg/tlf/man/Alignments.html)

- linetype:
  [`tlf::Linetypes`](https://rdrr.io/pkg/tlf/man/Linetypes.html)

For example, all parameters related to color (`titleColor`,
`yAxisLabelTicksColor`, etc.) accept any of the palettes available in
[`tlf::ColorMaps`](https://rdrr.io/pkg/tlf/man/ColorMaps.html) (e.g.
`tlf::ColorMaps$ospDefault`).

Note that these are named lists, and, therefore, if you want to assign a
specific element from a list to an object's public field, you will have
to extract that element first.

For example, if you want to specify that the legend position should be
outside the plot to the left and at bottom, you will have to do the
following:

    myPlotConfiguration <- DefaultPlotConfiguration$new()
    myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideBottomLeft

Of course, the extracted element doesn't have to be a single value, and
can also be an atomic vector. For example, if you want to assign a
different line type to each group in a profile plot, you will have to
assign a vector of line types.

    myPlotConfiguration <- DefaultPlotConfiguration$new()
    myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)

If there are more number of elements in the vector than the number of
groups, the additional elements will be ignored.

## Specifying units

The available units for `x`-and `y`-axes depend on the dimensions of
these quantities
([`ospsuite::ospDimensions`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospDimensions.md)).
Supported units can be seen with
[`ospsuite::ospUnits`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/ospUnits.md).

## Specifying fonts

A font is a particular set of glyphs (character shapes), differentiated
from other fonts in the same family by additional properties such as
stroke weight, slant, relative width, etc.

A font face (aka typeface) is the design of lettering, characterized by
variations in size, weight (e.g. bold), slope (e.g. italic), width (e.g.
condensed), and so on. The available font faces can seen using
[`tlf::FontFaces`](https://rdrr.io/pkg/tlf/man/FontFaces.html) list.

A font family is a grouping of fonts defined by shared design styles.

The available font families will depend on which fonts have been
installed on your computer. This information can be extracted by running
the following code:

    # install.packages("systemfonts")
    library(systemfonts)
    system_fonts()

## Specifying scaling

Transformations for both x- and y-axes can be (independently) specified.
The default is linear for both axes.

The available transformations can be seen in the
[`tlf::Scaling`](https://rdrr.io/pkg/tlf/man/Scaling.html) list.

## Specifying tick labels

[`tlf::TickLabelTransforms`](https://rdrr.io/pkg/tlf/man/TickLabelTransforms.html)
lists of all available tick label transformations. For example,
selecting `tlf::TickLabelTransforms$identity` will display tick labels
as they are, while selecting `tlf::TickLabelTransforms$log` will display
tick labels in logarithmic scale format.

## Saving plot

By default, the plots will be shown in plot pane of your IDE, but the
plots can also be saved to a file using the
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function.

    myPlot <- plotIndividualTimeProfile(myDataComb, myPC)
    ggplot2::ggsave(filename = "plot_1.png", plot = myPlot)

## See also

Other plotting:
[`plotIndividualTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotIndividualTimeProfile.md),
[`plotObservedVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotObservedVsSimulated.md),
[`plotPopulationTimeProfile()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotPopulationTimeProfile.md),
[`plotResidualsVsSimulated()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsSimulated.md),
[`plotResidualsVsTime()`](https://www.open-systems-pharmacology.org/OSPSuite-R/dev/reference/plotResidualsVsTime.md)

## Public fields

- `xUnit, yUnit`:

  Units for quantities plotted on x- and y-axes, respectively.

- `title, subtitle, caption, xLabel, yLabel, legendTitle, watermark`:

  A character string providing plot annotations for plot title,
  subtitle, caption, x-axis label, y-axis label, plot legend, watermark,
  respectively.

- `titleColor, titleSize, titleFontFace, titleFontFamily, titleAngle, titleAlign, titleMargin`:

  Aesthetic properties for the plot title.

- `subtitleColor, subtitleSize, subtitleFontFace, subtitleFontFamily, subtitleAngle, subtitleAlign, subtitleMargin`:

  Aesthetic properties for the plot subtitle.

- `captionColor, captionSize, captionFontFace, captionFontFamily, captionAngle, captionAlign, captionMargin`:

  Aesthetic properties for the plot caption.

- `xLabelColor, xLabelSize, xLabelFontFace, xLabelFontFamily, xLabelAngle, xLabelAlign, xLabelMargin`:

  Aesthetic properties for the plot xLabel.

- `yLabelColor, yLabelSize, yLabelFontFace, yLabelFontFamily, yLabelAngle, yLabelAlign, yLabelMargin`:

  Aesthetic properties for the plot yLabel.

- `legendPosition`:

  A character string defining the legend position. Available options can
  be seen using
  [`tlf::LegendPositions`](https://rdrr.io/pkg/tlf/man/LegendPositions.html)
  list.

- `legendTitleSize, legendTitleColor, legendTitleFontFamily, legendTitleFontFace, legendTitleAngle, legendTitleAlign, legendTitleMargin`:

  Aesthetic properties for the legend title.

- `legendKeysSize, legendKeysColor, legendKeysFontFamily, legendKeysFontFace, legendKeysAngle, legendKeysAlign, legendKeysMargin`:

  Aesthetic properties for the legend caption.

- `legendBackgroundColor, legendBackgroundAlpha, legendBorderColor, legendBorderType, legendBorderSize`:

  Aesthetic properties for the legend box

- `xAxisTicksLabels, xAxisLabelTicksSize, xAxisLabelTicksColor, xAxisLabelTicksFontFamily, xAxisLabelTicksFontFace, xAxisLabelTicksAngle, xAxisLabelTicksAlign, xAxisLabelTicksMargin, xAxisExpand`:

  Aesthetic properties for the x-axis label.

- `yAxisTicksLabels, yAxisLabelTicksSize, yAxisLabelTicksColor, yAxisLabelTicksFontFamily, yAxisLabelTicksFontFace, yAxisLabelTicksAngle, yAxisLabelTicksAlign, yAxisLabelTicksMargin, yAxisExpand`:

  Aesthetic properties for the y-axis label.

- `xAxisLimits, yAxisLimits`:

  A numeric vector of axis limits for the x-and y-axis, respectively.
  This will preserve all data points but zoom in the plot.

- `xValuesLimits, yValuesLimits`:

  A numeric vector of values limits for the x-and y-axis, respectively.
  This will filter out the data points outside the specified ranges
  before plotting.

- `xAxisTicks, yAxisTicks`:

  A numeric vector or a function defining where to position x-and y-axis
  ticks, respectively.

- `xAxisScale, yAxisScale`:

  A character string defining axis scale. Available options can be seen
  using [`tlf::Scaling`](https://rdrr.io/pkg/tlf/man/Scaling.html) list.

- `watermarkSize, watermarkColor, watermarkFontFamily, watermarkFontFace, watermarkAngle, watermarkAlign, watermarkMargin`:

  A character string specifying the aesthetic properties for the
  watermark.

- `plotBackgroundFill, plotBackgroundColor, plotBackgroundSize, plotBackgroundLinetype`:

  A character string specifying the aesthetic properties for the plot
  background.

- `plotPanelBackgroundFill, plotPanelBackgroundColor, plotPanelBackgroundSize, plotPanelBackgroundLinetype`:

  A character string specifying the aesthetic properties for the plot
  panel (inside of plot) background.

- `xAxisColor, xAxisSize, xAxisLinetype`:

  A character string specifying the aesthetic properties for the x-axis.

- `yAxisColor, yAxisSize, yAxisLinetype`:

  A character string specifying the aesthetic properties for the y-axis.

- `xGridColor, xGridSize, xGridLinetype`:

  A character string specifying the aesthetic properties for the x-axis
  grid.

- `yGridColor, yGridSize, yGridLinetype`:

  A character string specifying the aesthetic properties for the y-axis
  grid.

- `linesColor, linesSize, linesLinetype, linesAlpha`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, respectively, for lines.

- `pointsColor, pointsShape, pointsSize, pointsAlpha`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, respectively, for points.

- `ribbonsFill, ribbonsSize, ribbonsLinetype, ribbonsAlpha`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, respectively, for ribbons.

- `errorbarsSize, errorbarsLinetype, errorbarsAlpha, errorbarsCapSize`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, cap width/height, respectively, for error bars.

- `displayLLOQ`:

  A Boolean controlling display Lower Limit of Quantification lines.
  Default to True.

- `lloqDirection`:

  A string controlling how the LLOQ lines are plotted. Can be
  "vertical", "horizontal" or "both". Default to NULL to respect
  specific plot configurations.

- `foldLinesLegend`:

  A Boolean controlling the drawing of the fold lines in the legend.
  Default to False.

- `foldLinesLegendDiagonal`:

  A Boolean controlling whether the fold lines legend should be
  horizontal or diagonal lines.

## Active bindings

- `xUnit, yUnit`:

  Units for quantities plotted on x- and y-axes, respectively.

- `title, subtitle, caption, xLabel, yLabel, legendTitle, watermark`:

  A character string providing plot annotations for plot title,
  subtitle, caption, x-axis label, y-axis label, plot legend, watermark,
  respectively.

- `titleColor, titleSize, titleFontFace, titleFontFamily, titleAngle, titleAlign, titleMargin`:

  Aesthetic properties for the plot title.

- `subtitleColor, subtitleSize, subtitleFontFace, subtitleFontFamily, subtitleAngle, subtitleAlign, subtitleMargin`:

  Aesthetic properties for the plot subtitle.

- `captionColor, captionSize, captionFontFace, captionFontFamily, captionAngle, captionAlign, captionMargin`:

  Aesthetic properties for the plot caption.

- `xLabelColor, xLabelSize, xLabelFontFace, xLabelFontFamily, xLabelAngle, xLabelAlign, xLabelMargin`:

  Aesthetic properties for the plot xLabel.

- `yLabelColor, yLabelSize, yLabelFontFace, yLabelFontFamily, yLabelAngle, yLabelAlign, yLabelMargin`:

  Aesthetic properties for the plot yLabel.

- `legendTitleSize, legendTitleColor, legendTitleFontFamily, legendTitleFontFace, legendTitleAngle, legendTitleAlign, legendTitleMargin`:

  Aesthetic properties for the legend title.

- `legendKeysSize, legendKeysColor, legendKeysFontFamily, legendKeysFontFace, legendKeysAngle, legendKeysAlign, legendKeysMargin`:

  Aesthetic properties for the legend caption.

- `legendBackgroundColor, legendBackgroundAlpha, legendBorderColor, legendBorderType, legendBorderSize`:

  Aesthetic properties for the legend box

- `xAxisTicksLabels, xAxisLabelTicksSize, xAxisLabelTicksColor, xAxisLabelTicksFontFamily, xAxisLabelTicksFontFace, xAxisLabelTicksAngle, xAxisLabelTicksAlign, xAxisLabelTicksMargin, xAxisExpand`:

  Aesthetic properties for the x-axis label.

- `yAxisTicksLabels, yAxisLabelTicksSize, yAxisLabelTicksColor, yAxisLabelTicksFontFamily, yAxisLabelTicksFontFace, yAxisLabelTicksAngle, yAxisLabelTicksAlign, yAxisLabelTicksMargin, yAxisExpand`:

  Aesthetic properties for the y-axis label.

- `xAxisLimits, yAxisLimits`:

  A numeric vector of axis limits for the x-and y-axis, respectively.
  This will preserve all data points but zoom in the plot.

- `xValuesLimits, yValuesLimits`:

  A numeric vector of values limits for the x-and y-axis, respectively.
  This will filter out the data points outside the specified ranges
  before plotting.

- `xAxisTicks, yAxisTicks`:

  A numeric vector or a function defining where to position x-and y-axis
  ticks, respectively.

- `xAxisScale, yAxisScale`:

  A character string defining axis scale. Available options can be seen
  using [`tlf::Scaling`](https://rdrr.io/pkg/tlf/man/Scaling.html) list.

- `watermarkSize, watermarkColor, watermarkFontFamily, watermarkFontFace, watermarkAngle, watermarkAlign, watermarkMargin`:

  A character string specifying the aesthetic properties for the
  watermark.

- `plotBackgroundFill, plotBackgroundColor, plotBackgroundSize, plotBackgroundLinetype`:

  A character string specifying the aesthetic properties for the plot
  background.

- `plotPanelBackgroundFill, plotPanelBackgroundColor, plotPanelBackgroundSize, plotPanelBackgroundLinetype`:

  A character string specifying the aesthetic properties for the plot
  panel (inside of plot) background.

- `xAxisColor, xAxisSize, xAxisLinetype`:

  A character string specifying the aesthetic properties for the x-axis.

- `yAxisColor, yAxisSize, yAxisLinetype`:

  A character string specifying the aesthetic properties for the y-axis.

- `xGridColor, xGridSize, xGridLinetype`:

  A character string specifying the aesthetic properties for the x-axis
  grid.

- `yGridColor, yGridSize, yGridLinetype`:

  A character string specifying the aesthetic properties for the y-axis
  grid.

- `linesColor, linesSize, linesLinetype, linesAlpha`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, respectively, for lines.

- `pointsColor, pointsShape, pointsSize, pointsAlpha`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, respectively, for points.

- `ribbonsFill, ribbonsSize, ribbonsLinetype, ribbonsAlpha`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, respectively, for ribbons.

- `errorbarsSize, errorbarsLinetype, errorbarsAlpha, errorbarsCapSize`:

  A selection key or values for choice of color, fill, shape, size,
  linetype, alpha, cap width/height, respectively, for error bars.

## Methods

### Public methods

- [`DefaultPlotConfiguration$clone()`](#method-DefaultPlotConfiguration-clone)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DefaultPlotConfiguration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a new instance of this class
myPlotConfiguration <- DefaultPlotConfiguration$new()

# Change defaults
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$pointsSize <- 2.5
myPlotConfiguration$legendTitle <- "My Legend Title"

# Checking new values
myPlotConfiguration$pointsSize
#> [1] 2.5

# To check all default values, you can print the object
myPlotConfiguration
#> <DefaultPlotConfiguration>
#>   Public:
#>     caption: NULL
#>     captionAlign: right
#>     captionAngle: 0
#>     captionColor: black
#>     captionFontFace: plain
#>     captionFontFamily: 
#>     captionMargin: 2 2 5 2
#>     captionSize: 8
#>     clone: function (deep = FALSE) 
#>     displayLLOQ: TRUE
#>     errorbarsAlpha: 0.75
#>     errorbarsCapSize: 4
#>     errorbarsLinetype: solid
#>     errorbarsSize: 1
#>     foldLinesLegend: FALSE
#>     foldLinesLegendDiagonal: FALSE
#>     legendBackgroundAlpha: 0
#>     legendBackgroundColor: white
#>     legendBorderColor: NULL
#>     legendBorderSize: NULL
#>     legendBorderType: 1
#>     legendKeysAlign: left
#>     legendKeysAngle: 0
#>     legendKeysColor: black
#>     legendKeysFontFace: plain
#>     legendKeysFontFamily: 
#>     legendKeysMargin: 2 0 2 0
#>     legendKeysSize: 10
#>     legendPosition: NULL
#>     legendTitle: My Legend Title
#>     legendTitleAlign: left
#>     legendTitleAngle: 0
#>     legendTitleColor: black
#>     legendTitleFontFace: plain
#>     legendTitleFontFamily: 
#>     legendTitleMargin: 2 2 2 2
#>     legendTitleSize: 10
#>     linesAlpha: 0.75
#>     linesColor: NULL
#>     linesLinetype: NULL
#>     linesSize: 1
#>     lloqDirection: NULL
#>     plotBackgroundColor: black
#>     plotBackgroundFill: white
#>     plotBackgroundLinetype: blank
#>     plotBackgroundSize: 0.5
#>     plotPanelBackgroundColor: black
#>     plotPanelBackgroundFill: white
#>     plotPanelBackgroundLinetype: solid
#>     plotPanelBackgroundSize: 0.5
#>     pointsAlpha: 0.75
#>     pointsColor: #5050FFFF #CE3D32FF #749B58FF #F0E685FF #466983FF #BA633 ...
#>     pointsShape: circle diamond triangle square invertedTriangle cross th ...
#>     pointsSize: 2.5
#>     ribbonsAlpha: 0.5
#>     ribbonsFill: #5050FFFF #CE3D32FF #749B58FF #F0E685FF #466983FF #BA633 ...
#>     ribbonsLinetype: solid longdash dotted dashed twodash dotdash blank
#>     ribbonsSize: 1
#>     subtitle: NULL
#>     subtitleAlign: left
#>     subtitleAngle: 0
#>     subtitleColor: black
#>     subtitleFontFace: plain
#>     subtitleFontFamily: 
#>     subtitleMargin: 0 2 10 2
#>     subtitleSize: 10
#>     title: My Plot Title
#>     titleAlign: left
#>     titleAngle: 0
#>     titleColor: black
#>     titleFontFace: plain
#>     titleFontFamily: 
#>     titleMargin: 20 2 10 2
#>     titleSize: 12
#>     watermark: NULL
#>     watermarkAlign: center
#>     watermarkAngle: 30
#>     watermarkColor: grey40
#>     watermarkFontFace: plain
#>     watermarkFontFamily: 
#>     watermarkMargin: 1 1 1 1
#>     watermarkSize: 20
#>     xAxisColor: black
#>     xAxisExpand: FALSE
#>     xAxisLabelTicksAlign: center
#>     xAxisLabelTicksAngle: 0
#>     xAxisLabelTicksColor: black
#>     xAxisLabelTicksFontFace: plain
#>     xAxisLabelTicksFontFamily: 
#>     xAxisLabelTicksMargin: 2 2 2 2
#>     xAxisLabelTicksSize: 8
#>     xAxisLimits: NULL
#>     xAxisLinetype: solid
#>     xAxisScale: NULL
#>     xAxisSize: 0.5
#>     xAxisTicks: NULL
#>     xAxisTicksLabels: identity
#>     xGridColor: grey
#>     xGridLinetype: blank
#>     xGridSize: 0.25
#>     xLabel: NULL
#>     xLabelAlign: center
#>     xLabelAngle: 0
#>     xLabelColor: black
#>     xLabelFontFace: plain
#>     xLabelFontFamily: 
#>     xLabelMargin: 10 2 5 2
#>     xLabelSize: 10
#>     xUnit: NULL
#>     xValuesLimits: NULL
#>     yAxisColor: black
#>     yAxisExpand: FALSE
#>     yAxisLabelTicksAlign: center
#>     yAxisLabelTicksAngle: 90
#>     yAxisLabelTicksColor: black
#>     yAxisLabelTicksFontFace: plain
#>     yAxisLabelTicksFontFamily: 
#>     yAxisLabelTicksMargin: 2 2 2 2
#>     yAxisLabelTicksSize: 8
#>     yAxisLimits: NULL
#>     yAxisLinetype: solid
#>     yAxisScale: NULL
#>     yAxisSize: 0.5
#>     yAxisTicks: NULL
#>     yAxisTicksLabels: identity
#>     yGridColor: grey
#>     yGridLinetype: blank
#>     yGridSize: 0.25
#>     yLabel: NULL
#>     yLabelAlign: center
#>     yLabelAngle: 90
#>     yLabelColor: black
#>     yLabelFontFace: plain
#>     yLabelFontFamily: 
#>     yLabelMargin: 5 2 10 2
#>     yLabelSize: 10
#>     yUnit: NULL
#>     yValuesLimits: NULL
```
