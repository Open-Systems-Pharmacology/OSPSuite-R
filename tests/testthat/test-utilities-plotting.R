context(".createAxesLabels")

df <- dplyr::tibble(
  dataType = c(rep("simulated", 3), rep("observed", 3)),
  xValues = c(0, 14.482, 28.965, 0, 1, 2),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(1, 1, 1, 1, 1, 1),
  yUnit = "mol/ml",
  yDimension = ospDimensions$`Concentration (mass)`,
  yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
  molWeight = c(10, 10, 20, 20, 10, 10)
)

test_that("It returns NULL when arguments are missing", {
  expect_null(.createAxesLabels(df))
})

test_that("It returns NULL when data frame is empty", {
  expect_null(.createAxesLabels(data.frame(), TimeProfilePlotConfiguration$new()))
})

test_that("It replaces 'Concentration (molar)' and 'Concentration (mass)' by 'Concentration' in plot axis labels", {
  labels <- .createAxesLabels(.unitConverter(df), TimeProfilePlotConfiguration$new())

  expect_equal(labels$xLabel, "Time [min]")
  expect_equal(labels$yLabel, "Concentration [mol/ml]")
})


test_that("It works correctly when multiple dimensions are present", {
  concDataSet <- DataSet$new(name = "Concentration data set")
  concDataSet$setValues(1, 1)
  concDataSet$yDimension <- ospDimensions$`Concentration (molar)`
  concDataSet$molWeight <- 1

  amountDataSet <- DataSet$new(name = "Amount data set")
  amountDataSet$setValues(1, 1)
  amountDataSet$yDimension <- ospDimensions$`Concentration (mass)`
  amountDataSet$molWeight <- 1

  myCombDat <- DataCombined$new()
  myCombDat$addDataSets(c(concDataSet, amountDataSet))

  df <- myCombDat$toDataFrame()

  labs <- .createAxesLabels(.unitConverter(df), tlf::TimeProfilePlotConfiguration$new())

  expect_equal(labs$xLabel, "Time [h]")
  expect_equal(labs$yLabel, "Concentration [µmol/l]")
})
