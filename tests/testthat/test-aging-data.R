# AgingData

test_that("Creating an aging data and setting values works", {
  agingData <- AgingData$new()
  agingData$individualIds <- c(1, 2, 3)
  agingData$parameterPaths <- c("Path1", "Path2", "Path3")
  agingData$values <- c(1.5, 2.5, 3.54)
  agingData$times <- c(1, 2.5, 3)

  expect_equal(agingData$individualIds, c(1, 2, 3))
  expect_equal(agingData$parameterPaths, c("Path1", "Path2", "Path3"))
  expect_equal(agingData$values, c(1.5, 2.5, 3.54))
  expect_equal(agingData$times, c(1, 2.5, 3))
})
