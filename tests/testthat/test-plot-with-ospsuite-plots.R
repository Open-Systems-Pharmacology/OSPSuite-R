# Test for .getMappingForTimeprofiles function
test_that(".getMappingForTimeprofiles constructs mapping correctly", {

  # Mock plot data
  mockPlotData <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10, 20, 15, 25, 30),
    group = rep("A", 5),
    name = LETTERS[seq(1,5)],
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = "mg/l"
  )

  mockPlotDataNoGroup <- copy(mockPlotData)[, group := NA]

  # Mock metadata
  mockMetaData <- list(
    xValues = list(dimension = "Time", unit = "h"),
    yValues = list(dimension = "Concentration", unit = "mg/l")
  )

  # Test without user mapping for data with group
  mapping <- .getMappingForTimeprofiles(mockPlotData, mockMetaData, userMapping = NULL)
  expect_true("uneval" %in% class(mapping))
  expect_equal(rlang::as_label(mapping$x), 'xValues')
  expect_equal(rlang::as_label(mapping$y), 'yValues')
  expect_equal(rlang::as_label(mapping$groupby), "group")
  expect_equal(rlang::as_label(mapping$group), "interaction(group, name)")

  # Test without user mapping for data without group
  mapping <- .getMappingForTimeprofiles(mockPlotDataNoGroup, mockMetaData, userMapping = NULL)
  expect_equal(rlang::as_label(mapping$groupby), "name")
  expect_false('group' %in% names(mapping))

  # Test with user mapping for data with group
  userMapping <- ggplot2::aes(color = group, groupby = dataType)
  mappingWithUser <- .getMappingForTimeprofiles(mockPlotData, mockMetaData, userMapping = userMapping)
  expect_equal(rlang::as_label(mappingWithUser$colour), 'group')
  expect_equal(rlang::as_label(mappingWithUser$groupby), "dataType")
  expect_false('group' %in% names(mappingWithUser))

  # Test with yErrorType present
  mockPlotDataError <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10, 20, 15, 25, 30),
    group = rep("A", 5),
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = "mg/l",
    yErrorType = rep("ArithmeticStdDev", 5),
    yErrorValues = c(1, 2, 1.5, 2.5, 3)
  )

  mappingWithError <- .getMappingForTimeprofiles(mockPlotDataError, mockMetaData, userMapping = NULL)
  expect_true("error" %in% names(mappingWithError))

  # Test with yMin and yMax present
  mockPlotDataMinMax <- data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(10, 20, 15, 25, 30),
    group = rep("A", 5),
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = "mg/l",
    yMin = c(8, 18, 12, 22, 28),
    yMax = c(12, 22, 18, 28, 32)
  )

  mappingMinMax <- .getMappingForTimeprofiles(mockPlotDataMinMax, mockMetaData, userMapping = NULL)
  expect_true("ymin" %in% names(mappingMinMax))
  expect_true("ymax" %in% names(mappingMinMax))

  # Mock plot data with a secondary y-axis (y2)
  mockPlotDataWithY2 <- data.table(
    xValues = c(1, 2, 3, 1, 2),
    yValues = c(10, 20, 15, 0.1, 0.2),
    group = rep("A", 5),
    dataType = rep("observed", 5),
    xUnit = "h",
    yUnit = c(rep("mg/l",3), rep("ml",2))
  )

  # Mock metadata indicating presence of secondary y-axis
  mockMetaDataWithY2 <- list(
    xValues = list(dimension = "Time", unit = "h"),
    yValues = list(dimension = "Concentration", unit = "mg/l"),
    y2 = list(dimension = "Volume", unit = "ml")  # Secondary y-axis metadata
  )

  # Test mapping with y2
  mappingWithY2 <- .getMappingForTimeprofiles(mockPlotDataWithY2, mockMetaDataWithY2, userMapping = NULL)
  expect_contains(names(mappingWithY2), 'y2axis')

})
