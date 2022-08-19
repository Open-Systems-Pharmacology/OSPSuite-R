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
    title = "defaults - obs",
    fig = plotIndividualTimeProfile(oneObsDC)
  )
})

test_that("It creates default plots as expected for multiple observed datasets", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults - many - obs",
    fig = plotIndividualTimeProfile(manyObsDC)
  )
})

# only simulated ------------------------

test_that("It creates default plots as expected for single simulated dataset", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "defaults - sim",
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
    title = "defaults - both",
    fig = plotIndividualTimeProfile(oneObsSimDC)
  )
})

test_that("It respects custom plot configuration", {
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "custom",
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

test_that("It maps multiple observed datasets to different shapes", {
  dataSet1 <- DataSet$new(name = "Dataset1")
  dataSet1$setValues(1, 1)
  dataSet1$yDimension <- ospDimensions$`Concentration (molar)`
  dataSet1$molWeight <- 1

  dataSet2 <- DataSet$new(name = "Dataset2")
  dataSet2$setValues(2, 1)
  dataSet2$yDimension <- ospDimensions$`Concentration (mass)`
  dataSet2$molWeight <- 1

  dataSet3 <- DataSet$new(name = "Dataset3")
  dataSet3$setValues(1, 3)
  dataSet3$yDimension <- ospDimensions$`Concentration (mass)`
  dataSet3$molWeight <- 1

  myCombDat5 <- DataCombined$new()
  myCombDat5$addDataSets(
    c(dataSet1, dataSet2, dataSet3),
    groups = "myGroup"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "multiple obs",
    fig = plotIndividualTimeProfile(myCombDat5)
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
