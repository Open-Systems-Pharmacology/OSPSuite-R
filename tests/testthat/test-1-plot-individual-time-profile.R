# plotIndividualTimeProfile

# only observed ------------------------

test_that("It creates default plots as expected for single observed dataset", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "single obs",
    fig = plotIndividualTimeProfile(oneObsDCGlobal)
  )
})

test_that("It creates default plots as expected for multiple observed datasets", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs",
    fig = plotIndividualTimeProfile(manyObsDCGlobal)
  )
})

test_that("It plots multiple observed datasets with dataset name legend entries", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs - separate legend",
    fig = plotIndividualTimeProfile(
      manyObsDCGlobal,
      showLegendPerDataset = TRUE
    )
  )
})


# only simulated ------------------------

test_that("It creates default plots as expected for single simulated dataset", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "single sim",
    fig = plotIndividualTimeProfile(oneSimDCGlobal)
  )
})

test_that("It creates default plots as expected for multiple simulated datasets", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim",
    fig = plotIndividualTimeProfile(manySimDCGlobal)
  )
})

test_that("It plots multiple simulated datasets with dataset name legend entries", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple sim - separate legend",
    fig = plotIndividualTimeProfile(
      manySimDCGlobal,
      showLegendPerDataset = TRUE
    )
  )
})

# single observed and simulated datasets ------------------------

test_that("It creates default plots as expected for both observed and simulated", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - default",
    fig = plotIndividualTimeProfile(oneObsSimDCGlobal)
  )
})

test_that("It respects custom plot configuration", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - custom",
    fig = plotIndividualTimeProfile(oneObsSimDCGlobal, customDPCGlobal)
  )

  # Since these were not specified by the user, they should not be updated
  # after plotting function is done with it.
  expect_null(customDPCGlobal$xLabel)
  expect_null(customDPCGlobal$yLabel)
})

test_that("It plots both observed and simulated datasets with dataset name legend entries", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - separate legend",
    fig = plotIndividualTimeProfile(
      oneObsSimDCGlobal,
      showLegendPerDataset = TRUE
    )
  )
})

test_that("It plots both observed and simulated datasets with dataset name legend entries and custom plot configuration", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "both - custom - separate legend",
    fig = plotIndividualTimeProfile(
      oneObsSimDCGlobal,
      customDPCGlobal,
      showLegendPerDataset = TRUE
    )
  )
})

# multiple observed and simulated datasets ------------------------

test_that("It maps multiple observed and simulated datasets to different visual properties", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs and sim",
    fig = plotIndividualTimeProfile(manyObsSimDCGlobal)
  )
})

# edge cases ------------------------

test_that("It works when geometric error is present", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geometric error",
    fig = plotIndividualTimeProfile(oneObsGeometricDCGlobal)
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
    fig = plotIndividualTimeProfile(dc, defaultPlotConfiguration = noLLOQ_DPC)
  )
})
