# .initPackage / graceful-init state

test_that("getOSPSuiteSetting('initialized') reports a working runtime as TRUE", {
  # These tests run in an environment with a working .NET runtime, so a
  # successful initialisation must leave `initialized` TRUE and no load error.
  skip_if_not(
    isTRUE(getOSPSuiteSetting("initialized")),
    "Requires a working OSPSuite .NET runtime."
  )

  expect_type(getOSPSuiteSetting("initialized"), "logical")
  expect_true(getOSPSuiteSetting("initialized"))
  expect_null(ospsuiteEnv$loadError)
})

test_that(".initPackage() degrades gracefully when native loading fails", {
  # `.initPackage()` mutates the package-global `ospsuiteEnv`. Capture the
  # current state and restore it after the test so a simulated failure here
  # cannot corrupt state for other tests.
  originalInitialized <- ospsuiteEnv$initialized
  originalLoadError <- ospsuiteEnv$loadError
  withr::defer({
    ospsuiteEnv$initialized <- originalInitialized
    ospsuiteEnv$loadError <- originalLoadError
  })

  # Simulate a native-library load failure without a broken runtime. The error
  # is caught by the graceful-degradation `tryCatch` in `.initPackage()`.
  simulatedMessage <- "simulated native load failure"
  testthat::local_mocked_bindings(
    .loadNativeLibraries = function(libDir) stop(simulatedMessage)
  )

  # `.initPackage()` may emit an unrelated dev-environment warning about a
  # missing bundled DLL; it is not what this test is about, so suppress it.
  suppressWarnings(.initPackage())

  expect_false(ospsuiteEnv$initialized)
  expect_match(ospsuiteEnv$loadError, simulatedMessage, fixed = TRUE)
})
