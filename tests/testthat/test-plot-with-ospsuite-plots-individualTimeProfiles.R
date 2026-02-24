# Set defaults
oldDefaults <- ospsuite.plots::setDefaults()
withr::defer(ospsuite.plots::resetDefaults(oldDefaults))
ggplot2::theme_update(legend.title = ggplot2::element_blank())
ggplot2::theme_update(legend.position = c(0.95, 0.95))
ggplot2::theme_update(legend.justification = c("right", "top"))

# `DataCombined` objects ------------------------

oneObsDC <- readRDS(getTestDataFilePath("oneObsDC"))
manyObsDC <- readRDS(getTestDataFilePath("manyObsDC"))

oneSimDC <- readRDS(getTestDataFilePath("oneSimDC"))
manySimDC <- readRDS(getTestDataFilePath("manySimDC"))

oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))
manyObsSimDC <- readRDS(getTestDataFilePath("manyObsSimDC"))

oneObsGeometricDC <- readRDS(getTestDataFilePath("oneObsGeometricDC"))

manyObsSimDCWithFraction <- readRDS(getTestDataFilePath(
  "manyObsSimDCWithFraction"
))

### only observed ------------------------
test_that("It creates default plots as expected for single observed dataset", {
  set.seed(123)

  vdiffr::expect_doppelganger(
    title = "single obs",
    fig = plotTimeProfile(oneObsDC)
  )
})

test_that("It creates default plots as expected for multiple observed datasets", {
  set.seed(123)

  vdiffr::expect_doppelganger(
    title = "multiple obs",
    fig = plotTimeProfile(manyObsDC)
  )

  vdiffr::expect_doppelganger(
    title = "multiple obs - separate legend",
    fig = plotTimeProfile(manyObsDC, mapping = ggplot2::aes(groupby = name))
  )
  
  vdiffr::expect_doppelganger(
    title = "multiple obs - showLegendPerDataset all",
    fig = suppressWarnings(plotTimeProfile(manyObsDC, showLegendPerDataset = "all"))
  )
  
  vdiffr::expect_doppelganger(
    title = "multiple obs - showLegendPerDataset observed",
    fig = plotTimeProfile(manyObsDC, showLegendPerDataset = "observed")
  )
}) 

# only simulated ------------------------

test_that("It creates default plots as expected for single simulated dataset", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "single sim",
    fig = plotTimeProfile(oneSimDC)
  )
})

test_that("It plots multiple simulated datasets with dataset name legend entries", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim - separate legend",
    fig = plotTimeProfile(
      manySimDC,
      mapping = ggplot2::aes(
        group = name,
        linetype = name
      )
    )
  )
  
  vdiffr::expect_doppelganger(
    title = "multiple sim - showLegendPerDataset all",
    fig = suppressWarnings(plotTimeProfile(manySimDC, showLegendPerDataset = "all"))
  )
  
  vdiffr::expect_doppelganger(
    title = "multiple sim - showLegendPerDataset simulated",
    fig = plotTimeProfile(manySimDC, showLegendPerDataset = "simulated")
  )
})

# single observed and simulated datasets ------------------------

test_that("It creates default plots as expected for both observed and simulated", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - default",
    fig = plotTimeProfile(oneObsSimDC)
  )
})

# multiple observed and simulated datasets ------------------------

test_that("It creates default plot with group legend for multiple obs and sim", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "many obs sim - default",
    fig = plotTimeProfile(manyObsSimDC)
  )
})

test_that("It maps multiple observed and simulated datasets to different visual properties", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "many obs sim - name",
    fig = plotTimeProfile(
      manyObsSimDC,
      mapping = ggplot2::aes(linetype = name),
      observedMapping = ggplot2::aes(fill = name)
    )
  )
  
  vdiffr::expect_doppelganger(
    title = "many obs sim - showLegendPerDataset all",
    fig = plotTimeProfile(manyObsSimDC, showLegendPerDataset = "all")
  )
  
  vdiffr::expect_doppelganger(
    title = "many obs sim - showLegendPerDataset observed",
    fig = suppressWarnings(plotTimeProfile(manyObsSimDC, showLegendPerDataset = "observed"))
  )
  
  vdiffr::expect_doppelganger(
    title = "many obs sim - showLegendPerDataset simulated",
    fig = suppressWarnings(plotTimeProfile(manyObsSimDC, showLegendPerDataset = "simulated"))
  )
})

test_that("It applies yScale and yScaleArgs to multiple obs and sim datasets", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "many obs sim - log scale",
    fig = plotTimeProfile(
      manyObsSimDC,
      yScale = "log",
      yScaleArgs = list(limits = c(0.001, NA))
    )
  )
})

test_that("User-provided mappings override showLegendPerDataset", {
  set.seed(123)
  # User mapping should override internal showLegendPerDataset mapping
  vdiffr::expect_doppelganger(
    title = "user mapping overrides showLegendPerDataset",
    fig = suppressWarnings(plotTimeProfile(
      manyObsSimDC,
      showLegendPerDataset = "all",
      mapping = ggplot2::aes(color = name),
      observedMapping = ggplot2::aes(color = name, fill = name)
    ))
  )
})

# showLegendPerDataset parameter validation and warnings ------------------------

test_that("It warns when using deprecated logical values", {
  expect_warning(
    plotTimeProfile(manyObsDC, showLegendPerDataset = TRUE),
    "Using logical values for 'showLegendPerDataset' is deprecated"
  )
  
  expect_warning(
    plotTimeProfile(manyObsDC, showLegendPerDataset = FALSE),
    "Using logical values for 'showLegendPerDataset' is deprecated"
  )
})

test_that("It warns when showLegendPerDataset setting doesn't match data", {
  # Only observed data, but asking for simulated differentiation
  expect_warning(
    plotTimeProfile(manyObsDC, showLegendPerDataset = "simulated"),
    "showLegendPerDataset = \"simulated\" but no simulated data present"
  )
  
  # Only simulated data, but asking for observed differentiation
  expect_warning(
    plotTimeProfile(manySimDC, showLegendPerDataset = "observed"),
    "showLegendPerDataset = \"observed\" but no observed data present"
  )
})

test_that("It warns when user mapping overrides showLegendPerDataset", {
  # User overrides linetype for simulated
  expect_warning(
    plotTimeProfile(
      manySimDC,
      showLegendPerDataset = "simulated",
      mapping = ggplot2::aes(linetype = group)
    ),
    "user 'mapping' overrides linetype for simulated data"
  )
  
  # User overrides shape for observed
  expect_warning(
    plotTimeProfile(
      manyObsDC,
      showLegendPerDataset = "observed",
      observedMapping = ggplot2::aes(shape = group)
    ),
    "user 'observedMapping' overrides shape for observed data"
  )
})

test_that("It handles edge case: observed data with showLegendPerDataset simulated", {
  set.seed(123)
  # Should warn and produce plot with no per-dataset differentiation
  vdiffr::expect_doppelganger(
    title = "obs only - showLegendPerDataset simulated",
    fig = suppressWarnings(plotTimeProfile(manyObsDC, showLegendPerDataset = "simulated"))
  )
})

test_that("It handles edge case: simulated data with showLegendPerDataset observed", {
  set.seed(123)
  # Should warn and produce plot with no per-dataset differentiation
  vdiffr::expect_doppelganger(
    title = "sim only - showLegendPerDataset observed",
    fig = suppressWarnings(plotTimeProfile(manySimDC, showLegendPerDataset = "observed"))
  )
})

# edge cases ------------------------

test_that("It works when geometric error is present", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geometric error",
    fig = plotTimeProfile(oneObsGeometricDC)
  )
})

test_that("It returns error when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_error(plotTimeProfile(myCombDat), messages$plotNoDataAvailable())
})

# LLOQ ----

test_that("It plots LLOQ correctly on log scale", {
  set.seed(42)
  dataSet <- DataSet$new("ds with lloq")
  dataSet$setValues(
    1:9,
    c(10 * exp(1:-5) + rnorm(7, 0, .25), rep(0.075, 2)),
    c(abs(rnorm(7, 0, 0.2)), rep(NA, 2))
  )
  dataSet$LLOQ <- 0.15

  dc <- DataCombined$new()
  dc$addDataSets(dataSet)

  vdiffr::expect_doppelganger(
    title = "lloq",
    fig = plotTimeProfile(dc, yScale = "log")
  )
})

# 2 y-axis dimensions ----
test_that("It plots data with two y-axis dimensions (fraction and concentration)", {
  vdiffr::expect_doppelganger(
    title = "with_secAxis",
    fig = plotTimeProfile(manyObsSimDCWithFraction, yScale = "log") +
      ggplot2::theme(
        legend.position = c(0.95, 0.05),
        legend.justification = c("right", "bottom")
      )
  )
})
