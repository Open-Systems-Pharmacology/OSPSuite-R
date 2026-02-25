test_that("It can print parameter range", {
  paramRange <- ParameterRange$new()
  expectSnapshotPrint(paramRange)
})
