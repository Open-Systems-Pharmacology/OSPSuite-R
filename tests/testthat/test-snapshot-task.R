# Tests for `loadSimulationsFromSnapshot()`, the R wrapper around the SnapshotTask
# `LoadSimulationsFromSnapshot` API. It returns the simulations stored in a
# snapshot file as `Simulation` objects, or - when simulation names are supplied -
# only the matching simulations (case-sensitive match).
#
# KNOWN FAILURE: these tests currently FAIL on purpose. The computational core
# returns snapshot-loaded simulations without a `SimulationConfiguration` (it is
# `NULL`), so wrapping them into `Simulation` objects via `Simulation$new()`
# throws a `NullReferenceException` in `CreateSimulationBuilderFor`. The tests
# encode the intended behaviour and act as the acceptance criterion for the
# .NET-side fix tracked in:
#   https://github.com/Open-Systems-Pharmacology/PK-Sim/issues/3560
# Once the core attaches a configuration to snapshot-loaded simulations, these
# tests must pass unchanged.

snapshotFile <- getTestDataFilePath("test_snapshot.json")

test_that("loadSimulationsFromSnapshot returns every simulation in the snapshot", {
  simulations <- loadSimulationsFromSnapshot(snapshotFile)

  expect_length(simulations, 2)
  expect_true(all(vapply(
    simulations,
    function(sim) isOfType(sim, "Simulation"),
    FUN.VALUE = logical(1)
  )))
})

test_that("loadSimulationsFromSnapshot returns only simulations whose name matches", {
  allSimulations <- loadSimulationsFromSnapshot(snapshotFile)
  expect_gt(length(allSimulations), 0)
  existingName <- allSimulations[[1]]$name

  simulations <- loadSimulationsFromSnapshot(
    snapshotFile,
    simulationNames = existingName
  )

  expect_length(simulations, 1)
  expect_equal(simulations[[1]]$name, existingName)
})

test_that("loadSimulationsFromSnapshot returns nothing for a non-existing name", {
  simulations <- loadSimulationsFromSnapshot(
    snapshotFile,
    simulationNames = "ThisSimulationDoesNotExist"
  )

  expect_length(simulations, 0)
})

test_that("a simulation loaded from a snapshot can be run", {
  simulation <- loadSimulationsFromSnapshot(snapshotFile)[[1]]

  results <- runSimulations(simulation)[[1]]
  expect_true(isOfType(results, "SimulationResults"))
})

test_that("loadSimulationsFromSnapshot validates its arguments", {
  expect_error(
    loadSimulationsFromSnapshot(snapshotFile, simulationNames = 1),
    regexp = "expected <character>"
  )
  expect_error(
    loadSimulationsFromSnapshot("does_not_exist.json"),
    regexp = "does not exist"
  )
})
