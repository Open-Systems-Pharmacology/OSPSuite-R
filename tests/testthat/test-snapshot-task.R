# Tests for the MoBi.R SnapshotTask exposed through the R API via
# `GetSnapshotTask`. `LoadSimulationsFromSnapshot` returns the simulations
# stored in a MoBi snapshot file; when simulation names are supplied only the
# matching simulations are returned (ordinal, case-sensitive match).
# Mirrors MoBi.R.Tests `SnapshotTaskSpecs`.

snapshotFile <- normalizePath(
  getTestDataFilePath("snapshot_no_pksim_modules.json")
)

test_that("LoadSimulationsFromSnapshot returns every simulation in the snapshot", {
  task <- .getMoBiTaskFromCache("SnapshotTask")
  simulations <- task$call("LoadSimulationsFromSnapshot", snapshotFile)

  expect_gt(length(simulations), 0)
})

test_that("LoadSimulationsFromSnapshot returns only simulations whose name matches", {
  task <- .getMoBiTaskFromCache("SnapshotTask")
  existingName <- task$call(
    "LoadSimulationsFromSnapshot",
    snapshotFile
  )[[1]]$get("Name")

  simulations <- task$call(
    "LoadSimulationsFromSnapshot",
    snapshotFile,
    existingName
  )

  expect_length(simulations, 1)
  expect_equal(simulations[[1]]$get("Name"), existingName)
})

test_that("LoadSimulationsFromSnapshot returns nothing for a non-existing name", {
  task <- .getMoBiTaskFromCache("SnapshotTask")
  simulations <- task$call(
    "LoadSimulationsFromSnapshot",
    snapshotFile,
    "ThisSimulationDoesNotExist"
  )

  expect_length(simulations, 0)
})
