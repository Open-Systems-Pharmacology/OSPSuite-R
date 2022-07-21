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

test_that("It returns `NULL` when arguments are missing", {
  expect_null(.createAxesLabels(df))
})

test_that("It returns `NULL` when data frame is empty", {
  expect_null(.createAxesLabels(data.frame(), TimeProfilePlotConfiguration$new()))
})

test_that("It replaces 'Concentration (molar)' and 'Concentration (mass)' by 'Concentration' in plot axis labels", {
  labels <- .createAxesLabels(.unitConverter(df), TimeProfilePlotConfiguration$new())

  expect_equal(labels$xLabel, "Time [min]")
  expect_equal(labels$yLabel, "Concentration [mol/ml]")
})


test_that("It works correctly when multiple dimensions are present and max frequency is not a tie", {
  concentrationMolarDataSet <- DataSet$new(name = "Concentration data set")
  concentrationMolarDataSet$setValues(1, 1)
  concentrationMolarDataSet$yDimension <- ospDimensions$`Concentration (molar)`
  concentrationMolarDataSet$molWeight <- 1

  concentrationMassDataSet <- DataSet$new(name = "Amount data set")
  concentrationMassDataSet$setValues(1, 1)
  concentrationMassDataSet$yDimension <- ospDimensions$`Concentration (mass)`
  concentrationMassDataSet$molWeight <- 1

  concentrationMassDataSet2 <- DataSet$new(name = "Amount data set 2")
  concentrationMassDataSet2$setValues(1, 1)
  concentrationMassDataSet2$yDimension <- ospDimensions$`Concentration (mass)`
  concentrationMassDataSet2$molWeight <- 1

  myCombDat <- DataCombined$new()
  myCombDat$addDataSets(c(concentrationMolarDataSet, concentrationMassDataSet, concentrationMassDataSet2))

  df <- myCombDat$toDataFrame()
  labs <- .createAxesLabels(.unitConverter(df), tlf::TimeProfilePlotConfiguration$new())

  expect_equal(labs$xLabel, "Time [h]")
  expect_equal(labs$yLabel, "Concentration [mg/l]")
})

context(".convertGeneralToSpecificPlotConfiguration")

test_that("It returns correct subclass instance of `PlotConfiguration`", {
  expect_s3_class(
    .convertGeneralToSpecificPlotConfiguration(
      tlf::TimeProfilePlotConfiguration$new(),
      DefaultPlotConfiguration$new()
    ),
    "TimeProfilePlotConfiguration"
  )

  expect_s3_class(
    .convertGeneralToSpecificPlotConfiguration(
      tlf::ResVsPredPlotConfiguration$new(),
      DefaultPlotConfiguration$new()
    ),
    "ResVsPredPlotConfiguration"
  )
})


context(".addMissingGroupings")

test_that("It adds dataset names as groups when grouping is missing", {
  df <- dplyr::tibble(
    group = c(
      "Stevens 2012 solid total",
      "Stevens 2012 solid total",
      NA,
      NA,
      NA
    ),
    name = c(
      "Organism|Lumen|Stomach|Metformin|Gastric retention",
      "Stevens_2012_placebo.Placebo_total",
      "Stevens_2012_placebo.Sita_dist",
      "Stevens_2012_placebo.Sita_proximal",
      "Stevens_2012_placebo.Sita_total"
    ),
    dataType = c(
      "simulated",
      "observed",
      "observed",
      "observed",
      "observed"
    )
  )

  df_new <- .addMissingGroupings(df)

  missing_idx <- which(is.na(df$group))

  expect_equal(
    df$name[missing_idx],
    df_new$group[missing_idx]
  )
})
