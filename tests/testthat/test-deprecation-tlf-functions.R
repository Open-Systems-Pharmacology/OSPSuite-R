# Soft deprecation warnings for tlf-based plotting functions

oneObsDC <- readRDS(getTestDataFilePath("oneObsDC"))
oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))

# plotIndividualTimeProfile -----------------------------------------------

test_that("plotIndividualTimeProfile emits a soft deprecation warning pointing to plotTimeProfile", {
  expect_warning(
    plotIndividualTimeProfile(oneObsDC),
    regexp = "plotIndividualTimeProfile.*deprecated.*plotTimeProfile",
    class = "lifecycle_warning_deprecated"
  )
})

# plotPopulationTimeProfile -----------------------------------------------

test_that("plotPopulationTimeProfile emits a soft deprecation warning pointing to plotTimeProfile", {
  expect_warning(
    plotPopulationTimeProfile(oneObsDC),
    regexp = "plotPopulationTimeProfile.*deprecated.*plotTimeProfile",
    class = "lifecycle_warning_deprecated"
  )
})

# plotObservedVsSimulated -------------------------------------------------

test_that("plotObservedVsSimulated emits a soft deprecation warning pointing to plotPredictedVsObserved", {
  expect_warning(
    try(plotObservedVsSimulated(oneObsSimDC), silent = TRUE),
    regexp = "plotObservedVsSimulated.*deprecated.*plotPredictedVsObserved",
    class = "lifecycle_warning_deprecated"
  )
})

# plotResidualsVsTime -----------------------------------------------------

test_that("plotResidualsVsTime emits a soft deprecation warning pointing to plotResidualsVsCovariate", {
  expect_warning(
    try(plotResidualsVsTime(oneObsSimDC), silent = TRUE),
    regexp = "plotResidualsVsTime.*deprecated.*plotResidualsVsCovariate",
    class = "lifecycle_warning_deprecated"
  )
})

# plotResidualsVsSimulated ------------------------------------------------

test_that("plotResidualsVsSimulated emits a soft deprecation warning pointing to plotResidualsVsCovariate", {
  expect_warning(
    try(plotResidualsVsSimulated(oneObsSimDC), silent = TRUE),
    regexp = "plotResidualsVsSimulated.*deprecated.*plotResidualsVsCovariate",
    class = "lifecycle_warning_deprecated"
  )
})
