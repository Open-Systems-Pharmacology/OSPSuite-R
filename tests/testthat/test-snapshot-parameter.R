test_that("It can print snapshot parameter", {
  snapshot_parameter <- SnapshotParameter$new()
  expect_snapshot(print(snapshot_parameter))
})
