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
