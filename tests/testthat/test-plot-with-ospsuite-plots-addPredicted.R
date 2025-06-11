test_that("Extrapolation for increasing yValues works correctly", {

  # Example data for testing
  dtSimulated <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(2, 4, 6, 8, 10),
    plotTag = rep("A", 5),
    outputPathId = rep("1", 5)
  )

  dtObserved <- data.table(
    xValues = c(1.5, 2.5, 3.5, 4.5),
    plotTag = rep("A", 4),
    outputPathId = rep("1", 4)
  )

  result <- addPredictedValues(dtObserved, dtSimulated, c("plotTag", "outputPathId"))

  expect_equal(result$predicted, c(3, 5, 7, 9)) # Expected linear predictions
})

test_that("Logarithmic interpolation for decreasing yValues works correctly", {

  dtSimulatedDecreasing <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10000, 100, 1, 0.01, 0.0001),
    plotTag = rep("B", 5),
    outputPathId = rep("1", 5)
  )

  dtObservedDecreasing <- data.table(
    xValues = c(1.5, 2.5, 3.5, 4.5),
    plotTag = rep("B", 4),
    outputPathId = rep("1", 4)
  )

  result <- addPredictedValues(dtObservedDecreasing, dtSimulatedDecreasing, c("plotTag", "outputPathId"))

  expect_equal(result$predicted, c(1000, 10, 0.1, 0.001)) # Expected log predictions
})

# Test with insufficient data
test_that("Function handles insufficient data gracefully", {
  dtSimulatedInsufficient <- data.table(
    xValues = c(1),
    yValues = c(2),
    plotTag = rep("C", 1),
    outputPathId = rep("1", 1)
  )

  dtObservedInsufficient <- data.table(
    xValues = c(1.5),
    plotTag = rep("C", 1),
    outputPathId = rep("1", 1)
  )

  result <- addPredictedValues(dtObservedInsufficient, dtSimulatedInsufficient, c("plotTag", "outputPathId"))

  expect_true(is.na(result$predicted))
})

test_that("Function handles mixed trends correctly", {

  # Test with mixed data
  dtSimulatedMixed <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(1, 3, 5, 4, 2), # Mixed increasing and decreasing
    plotTag = rep("D", 5),
    outputPathId = rep("1", 5)
  )

  dtObservedMixed <- data.table(
    xValues = c(1.5, 2.5, 3.5, 4.5),
    plotTag = rep("D", 4),
    outputPathId = rep("1", 4)
  )

  result <- addPredictedValues(dtObservedMixed, dtSimulatedMixed, c("plotTag", "outputPathId"))

  expect_equal(result$predicted, c(2, 4, exp(mean(c(log(5), log(4)))), exp(mean(c(log(2), log(4)))))) # Expected log predictions

  dtObservedMixed <- data.table(
    xValues = c(2, 3, 4, 5),
    plotTag = rep("D", 4),
    outputPathId = rep("1", 4)
  )

  result <- addPredictedValues(dtObservedMixed, dtSimulatedMixed, c("plotTag", "outputPathId"))

  expect_equal(result$predicted, c(3, 5, 4, 2)) # Expected log predictions
})


test_that("Extrapolation for negative yValues works correctly", {
  # Example data for testing
  dtSimulated <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(-2, -4, -6, -8, -10),
    plotTag = rep("A", 5),
    outputPathId = rep("1", 5)
  )

  dtObserved <- data.table(
    xValues = c(1.5, 2.5, 3.5, 4.5),
    plotTag = rep("A", 4),
    outputPathId = rep("1", 4)
  )

  result <- addPredictedValues(dtObserved, dtSimulated, c("plotTag", "outputPathId"))

  expect_equal(result$predicted, c(-3, -5, -7, -9)) # Expected linear predictions
})

test_that("Extrapolation for more then one group", {
  # Example data for testing
  dtSimulated <- data.table(
    xValues = rep(seq(1, 5), 4),
    yValues = seq(1, 20),
    plotTag = rep(c("A", "B", "A", "B"), each = 5),
    outputPathId = rep(c("1", "2"), each = 10)
  )

  dtObserved <- data.table(
    xValues = rep(c(1.5, 2.5, 3.5, 4.5), 4),
    plotTag = rep(c("A", "B", "A", "B"), each = 4),
    outputPathId = rep(c("1", "2"), each = 8)
  )

  result <- addPredictedValues(dtObserved, dtSimulated, c("plotTag", "outputPathId"))

  expect_equal(result$predicted, setdiff(seq(1.5, 20.5), seq(5.5, 20.5, 5))) # Expected linear predictions
})

