context("ospsuiteEnv")

test_that("It returns a value of a setting", {
  expect_equal(getOSPSuiteSetting("suiteVersion"), ospsuiteEnv$suiteVersion)
})

test_that("It returns a value of a nested setting", {
  expect_equal(
    getOSPSuiteSetting("sensitivityAnalysisConfig")$totalSensitivityThreshold,
    ospsuiteEnv$sensitivityAnalysisConfig$totalSensitivityThreshold
  )
})


test_that("It throws an error when the setting does not exist", {
  expect_error(getOSPSuiteSetting("someSetting"))
})
