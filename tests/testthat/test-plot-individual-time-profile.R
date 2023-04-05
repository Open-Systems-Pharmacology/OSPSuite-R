context("plotIndividualTimeProfile")
skip_on_os("linux") # `loadDataSetsFromExcel()` does not work for non-Windows platforms
skip_if(getRversion() < "4.1")

# `DataCombined` objects ------------------------

oneObsDC <- readRDS(getTestDataFilePath("oneObsDC"))
manyObsDC <- readRDS(getTestDataFilePath("manyObsDC"))

oneSimDC <- readRDS(getTestDataFilePath("oneSimDC"))
manySimDC <- readRDS(getTestDataFilePath("manySimDC"))

oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))
manyObsSimDC <- readRDS(getTestDataFilePath("manyObsSimDC"))

oneObsGeometricDC <- readRDS(getTestDataFilePath("oneObsGeometricDC"))

customDPC <- readRDS(getTestDataFilePath("customDPC"))

# only observed ------------------------

test_that("It creates default plots as expected for single observed dataset", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "single obs",
    fig = plotIndividualTimeProfile(oneObsDC)
  )
})

test_that("It creates default plots as expected for multiple observed datasets", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs",
    fig = plotIndividualTimeProfile(manyObsDC)
  )
})

# only simulated ------------------------

test_that("It creates default plots as expected for single simulated dataset", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "single sim",
    fig = plotIndividualTimeProfile(oneSimDC)
  )
})

test_that("It creates default plots as expected for multiple simulated datasets", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim",
    fig = plotIndividualTimeProfile(manySimDC)
  )
})

# single observed and simulated datasets ------------------------

test_that("It creates default plots as expected for both observed and simulated", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - default",
    fig = plotIndividualTimeProfile(oneObsSimDC)
  )
})

test_that("It respects custom plot configuration", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - custom",
    fig = plotIndividualTimeProfile(oneObsSimDC, customDPC)
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(customDPC$xLabel)
  expect_null(customDPC$yLabel)
})

# multiple observed and simulated datasets ------------------------

test_that("It maps multiple observed and simulated datasets to different visual properties", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs and sim",
    fig = plotIndividualTimeProfile(manyObsSimDC)
  )
})

# edge cases ------------------------

test_that("It works when geometric error is present", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geometric error",
    fig = plotIndividualTimeProfile(oneObsGeometricDC)
  )
})

test_that("It returns `NULL` when `DataCombined` is empty", {
  myCombDat <- DataCombined$new()

  expect_null(suppressWarnings(plotIndividualTimeProfile(myCombDat)))
  expect_warning(
    plotIndividualTimeProfile(myCombDat),
    messages$plottingWithEmptyDataCombined()
  )
})

# LLOQ

test_that("LLOQ is plotted", {
  dataSet <- DataSet$new("ds with lloq")
  dataSet$setValues(1:7, c(10 * exp(-1:-7) + rnorm(7, 0, .25)), abs(rnorm(7, 0, 0.1)))
  dataSet$LLOQ <- 0.15

  dc <- DataCombined$new()
  dc$addDataSets(dataSet)

  vdiffr::expect_doppelganger(
    title = "lloq",
    fig = plotIndividualTimeProfile(dc)
  )

  noLLOQ <- DefaultPlotConfiguration$new()
  noLLOQ$displayLLOQ = FALSE

  vdiffr::expect_doppelganger(
    title = "no lloq",
    fig = plotIndividualTimeProfile(dc, defaultPlotConfiguration = noLLOQ)
  )

})

