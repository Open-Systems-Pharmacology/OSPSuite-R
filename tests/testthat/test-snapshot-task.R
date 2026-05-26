# Tests for the SnapshotTask exposed through the R API as `GetSnapshotTask`,
# covering `LoadSimulationsFromSnapshot`: it returns the simulations stored in a
# snapshot file, or - when simulation names are supplied - only the matching
# simulations (ordinal, case-sensitive match). The workflow is mirrored across
# the MoBi.R and PKSim.R APIs.

# ---- MoBi.R SnapshotTask (MoBi snapshot) ----

mobiSnapshotFile <- normalizePath(
  getTestDataFilePath("snapshot_no_pksim_modules.json")
)

test_that("MoBi.R LoadSimulationsFromSnapshot returns every simulation in the snapshot", {
  task <- .getMoBiTaskFromCache("SnapshotTask")
  simulations <- task$call("LoadSimulationsFromSnapshot", mobiSnapshotFile)

  expect_length(simulations, 1)
})

test_that("MoBi.R LoadSimulationsFromSnapshot returns only simulations whose name matches", {
  task <- .getMoBiTaskFromCache("SnapshotTask")
  existingName <- task$call(
    "LoadSimulationsFromSnapshot",
    mobiSnapshotFile
  )[[1]]$get("Name")

  simulations <- task$call(
    "LoadSimulationsFromSnapshot",
    mobiSnapshotFile,
    existingName
  )

  expect_length(simulations, 1)
  expect_equal(simulations[[1]]$get("Name"), existingName)
})

test_that("MoBi.R LoadSimulationsFromSnapshot returns nothing for a non-existing name", {
  task <- .getMoBiTaskFromCache("SnapshotTask")
  simulations <- task$call(
    "LoadSimulationsFromSnapshot",
    mobiSnapshotFile,
    "ThisSimulationDoesNotExist"
  )

  expect_length(simulations, 0)
})

# ---- PKSim.R SnapshotTask (PK-Sim snapshot) ----

pksimSnapshotFile <- normalizePath(
  getTestDataFilePath("test_snapshot.json")
)

test_that("PKSim.R LoadSimulationsFromSnapshot returns every simulation in the snapshot", {
  initPKSim()
  task <- rSharp::callStatic("PKSim.R.Api, PKSim.R", "GetSnapshotTask")
  simulations <- task$call("LoadSimulationsFromSnapshot", pksimSnapshotFile)

  expect_length(simulations, 2)
})

test_that("PKSim.R LoadSimulationsFromSnapshot returns only simulations whose name matches", {
  initPKSim()
  task <- rSharp::callStatic("PKSim.R.Api, PKSim.R", "GetSnapshotTask")
  existingName <- task$call(
    "LoadSimulationsFromSnapshot",
    pksimSnapshotFile
  )[[1]]$get("Name")

  simulations <- task$call(
    "LoadSimulationsFromSnapshot",
    pksimSnapshotFile,
    existingName
  )

  expect_length(simulations, 1)
  expect_equal(simulations[[1]]$get("Name"), existingName)
})

test_that("PKSim.R LoadSimulationsFromSnapshot returns nothing for a non-existing name", {
  initPKSim()
  task <- rSharp::callStatic("PKSim.R.Api, PKSim.R", "GetSnapshotTask")
  simulations <- task$call(
    "LoadSimulationsFromSnapshot",
    pksimSnapshotFile,
    "ThisSimulationDoesNotExist"
  )

  expect_length(simulations, 0)
})
