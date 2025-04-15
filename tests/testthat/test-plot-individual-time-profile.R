# plotIndividualTimeProfile

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
  set.seed(42)
  dataSet <- DataSet$new("ds with lloq")
  dataSet$setValues(
    1:7,
    c(10 * exp(1:-5) + rnorm(7, 0, .25)),
    abs(rnorm(7, 0, 0.1))
  )
  dataSet$LLOQ <- 0.15

  dc <- DataCombined$new()
  dc$addDataSets(dataSet)

  LLOQ_DPC <- DefaultPlotConfiguration$new()
  LLOQ_DPC$lloqDirection <- "both"

  vdiffr::expect_doppelganger(
    title = "lloq",
    fig = plotIndividualTimeProfile(dc, defaultPlotConfiguration = LLOQ_DPC)
  )

  noLLOQ_DPC <- DefaultPlotConfiguration$new()
  noLLOQ_DPC$displayLLOQ <- FALSE

  vdiffr::expect_doppelganger(
    title = "no lloq",
    fig = plotIndividualTimeProfile(
      dc,
      defaultPlotConfiguration = noLLOQ_DPC
    )
  )
})

# Name mapping in legend

test_that("Name mapping in legend can be controlled", {
  # Create a test configuration with suppressNameInLegend set to FALSE
  showNameConfig <- DefaultPlotConfiguration$new()
  showNameConfig$suppressNameInLegend <- FALSE

  # Create a test configuration with suppressNameInLegend set to TRUE (default)
  hideNameConfig <- DefaultPlotConfiguration$new()
  hideNameConfig$suppressNameInLegend <- TRUE

  set.seed(123)

  # Test with name visible in legend
  vdiffr::expect_doppelganger(
    title = "name shown in legend",
    fig = plotIndividualTimeProfile(
      manyObsSimDC,
      defaultPlotConfiguration = showNameConfig
    )
  )

  # Test with name hidden in legend (default behavior)
  vdiffr::expect_doppelganger(
    title = "name hidden in legend",
    fig = plotIndividualTimeProfile(
      manyObsSimDC,
      defaultPlotConfiguration = hideNameConfig
    )
  )
})
