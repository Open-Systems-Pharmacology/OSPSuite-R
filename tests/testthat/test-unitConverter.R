context(".unitConverter")

# data -------------------

# small data frame to illustrate the conversion
df <- dplyr::tibble(
  xValues = c(15, 30, 60), xUnit = "min", xDimension = "Time",
  yValues = c(0.25, 45, 78), yUnit = c("", "%", "%"), yDimension = c("Fraction", "Fraction", "Fraction"),
  molWeight = c(10, 10, 10)
)

# also contains error columns
dfError <- dplyr::tibble(
  xValues = c(15, 30, 60), xUnit = "min", xDimension = "Time",
  yValues = c(0.25, 45, 78), yUnit = c("", "%", "%"), yDimension = c("Fraction", "Fraction", "Fraction"),
  yErrorValues = c(0.01, 5, 8), yErrorUnit = c("", "%", "%"),
  molWeight = c(10, 10, 10)
)

# default conversion -------------------

dfConvert <- .unitConverter(df)

test_that("defaults for .unitConverter update xValues column as expected", {
  expect_equal(dfConvert$xValues, df$xValues)
})

test_that("defaults for .unitConverter update yValues column as expected", {
  expect_equal(dfConvert$yValues[1], df$yValues[1])
  expect_equal(dfConvert$yValues[2:3] * 100, df$yValues[2:3])
})

test_that("defaults for .unitConverter update xUnit and yUnit column as expected", {
  expect_equal(unique(dfConvert$xUnit), unique(dfConvert$xUnit))
  expect_equal(unique(dfConvert$yUnit), unique(dfConvert$yUnit)[1])

  expect_equal(unique(dfConvert$xUnit), "min")
  expect_equal(unique(dfConvert$yUnit), "")
})

test_that("defaults for .unitConverter leaves dimension columns untouched", {
  expect_equal(unique(dfConvert$xDimension), "Time")
  expect_equal(unique(dfConvert$yDimension), "Fraction")
})

test_that("defaults for .unitConverter leaves molWeight columns untouched", {
  expect_equal(dfConvert$molWeight, df$molWeight)
})

# only `xUnit` -------------------

dfXConvert <- .unitConverter(df, xUnit = ospUnits$Time$h)

test_that(".unitConverter converts xValues column as expected", {
  expect_equal(dfXConvert$xValues, df$xValues / 60) # 1 hour = 60 minutes
})

test_that(".unitConverter updates xUnit column as expected", {
  expect_equal(unique(dfXConvert$xUnit), ospUnits$Time$h)
})

# only `yUnit` -------------------

dfYConvert <- .unitConverter(df, yUnit = ospUnits$Fraction$`%`)

test_that(".unitConverter converts yValues as expected", {
  expect_equal(dfYConvert$yValues[1], df$yValues[1] * 100)
  expect_equal(dfYConvert$yValues[2:3], df$yValues[2:3])
})

test_that(".unitConverter updates yUnit column as expected", {
  expect_equal(unique(dfYConvert$yUnit), ospUnits$Fraction$`%`)
})

# both `xUnit` and `yUnit` -------------------

dfXYConvert <- .unitConverter(df, xUnit = ospUnits$Time$h, yUnit = ospUnits$Fraction$`%`)

test_that(".unitConverter converts both xValues and yValues columns as expected", {
  expect_equal(dfXYConvert$xValues, df$xValues / 60)
  expect_equal(dfXYConvert$yValues[1], df$yValues[1] * 100)
  expect_equal(dfXYConvert$yValues[2:3], df$yValues[2:3])
})

test_that(".unitConverter updates xUnit and yUnit columns as expected", {
  expect_equal(unique(dfXYConvert$xUnit), ospUnits$Time$h)
  expect_equal(unique(dfXYConvert$yUnit), ospUnits$Fraction$`%`)
})

# molecular weight is used properly -------------------

dfMW <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(1, 2, 3),
  yUnit = c("mol", "mol", "mol"),
  yDimension = ospDimensions$Amount,
  molWeight = 10,
  random = "bla" # only for testing that the function doesn't remove other columns
)

dfMWConvert <- .unitConverter(dfMW, yUnit = ospUnits$Mass$g)

test_that(".unitConverter updates yValues with molecular weight when dimension is amount", {
  expect_equal(
    unique(dfMWConvert$yValues),
    toUnit(
      ospDimensions$Amount,
      values = dfMW$yValues,
      targetUnit = "g",
      sourceUnit = "mol",
      molWeight = dfMW$molWeight[[1]],
      molWeightUnit = ospUnits$`Molecular weight`$`g/mol`
    )
  )
})

test_that(".unitConverter updates yUnit correctly when dimension is amount", {
  expect_equal(unique(dfMWConvert$yUnit), ospUnits$Mass$g)
})

# dimensions don't change -------------------

test_that(".unitConverter doesn't change dimensions under any circumstances", {
  expect_equal(dim(dfConvert), dim(df))
  expect_equal(dim(dfXConvert), dim(df))
  expect_equal(dim(dfYConvert), dim(df))
  expect_equal(dim(dfXYConvert), dim(df))

  expect_equal(dim(dfMWConvert), dim(dfMW))
})

# error units -------------------

dfErrorConvert <- .unitConverter(dfError)
dfYErrorConvert <- .unitConverter(dfError, yUnit = ospUnits$Fraction$`%`)

test_that(".unitConverter changes error units as well - defaults", {
  expect_equal(unique(dfErrorConvert$yErrorUnit), "")
  expect_equal(dfErrorConvert$yErrorValues[1], dfError$yErrorValues[1])
  expect_equal(dfErrorConvert$yErrorValues[2:3] * 100, dfError$yErrorValues[2:3])
})

test_that(".unitConverter changes error units as well - defaults", {
  expect_equal(unique(dfYErrorConvert$yErrorUnit), "%")
  expect_equal(dfYErrorConvert$yErrorValues[1] / 100, dfError$yErrorValues[1])
  expect_equal(dfYErrorConvert$yErrorValues[2:3], dfError$yErrorValues[2:3])
})

# different molecular weights handled properly -------------------

test_that("Correct conversion for yValues having the same unit but different MW", {
  df <- dplyr::tibble(
    xValues = c(15, 30, 60), xUnit = "min", xDimension = "Time",
    yValues = c(1, 1, 1), yUnit = c("mol", "mol", "mol"), yDimension = c(ospDimensions$Amount, ospDimensions$Amount, ospDimensions$Amount),
    molWeight = c(10, 10, 20)
  )
  dfConvert <- .unitConverter(df, yUnit = "g")

  expect_equal(dfConvert$yValues, c(10, 10, 20))
})

# missing values handled properly -------------------

dfNA <- dplyr::tibble(
  dataType = c(rep("simulated", 3), rep("observed", 3)),
  xValues = c(0, 14.482, 28.965, 0, 1, 2),
  xUnit = "min", xDimension = "Time",
  yValues = c(25.579, 32.446, 32.103, 0, 0.995, 0.991),
  yUnit = c("%", "%", "%", "", "", ""),
  yDimension = "Fraction",
  yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
  yErrorUnit = c("%", "%", "%", NA, NA, NA),
  molWeight = c(NA, NA, NA, 129.1636, 129.1636, 129.1636)
)

dfNAConvert <- .unitConverter(dfNA)
dfNAXYConvert <- .unitConverter(dfNA, xUnit = "h", yUnit = "")

test_that("simulated data is as expected in presence of missing values - default units", {
  # because of the missing values, no conversion should have taken place and so these two
  # data frames should be identical
  expect_equal(
    dplyr::filter(dfNAConvert, dataType == "simulated"),
    dplyr::filter(dfNA, dataType == "simulated")
  )
})

test_that("observed data is as expected in presence of missing values - default units", {
  # only `yValues` should change; everything else should remain the same
  expect_equal(
    dplyr::filter(dfNAConvert, dataType == "observed"),
    dplyr::filter(dfNA, dataType == "observed") %>%
      dplyr::mutate(yValues = 100 * yValues, yUnit = "%")
  )
})

test_that("data have expected units in presence of missing values - custom units", {
  expect_equal(unique(dfNAXYConvert$xUnit), "h")
  expect_equal(unique(dfNAXYConvert$yUnit), "")
})

test_that("data have expected dimensions in presence of missing values - custom units", {
  expect_equal(unique(dfNAXYConvert$xDimension), "Time")
  expect_equal(unique(dfNAXYConvert$yDimension), "Fraction")
})

test_that("simulated xValues are converted as expected in presence of missing values - custom units", {
  expect_equal(
    dfNAXYConvert[dfNAXYConvert$dataType == "simulated", ]$xValues,
    c(0.0000000, 0.2413667, 0.4827500),
    tolerance = 0.001
  )
})

test_that("observed xValues are converted as expected in presence of missing values - custom units", {
  expect_equal(
    dfNAXYConvert[dfNAXYConvert$dataType == "observed", ]$xValues,
    c(0, 0.0166667, 0.0333333),
    tolerance = 0.001
  )
})

test_that("simulated yValues are converted as expected in presence of missing values - custom units", {
  expect_equal(
    dfNAXYConvert[dfNAXYConvert$dataType == "simulated", ]$yValues,
    c(0.25579, 0.32446, 0.32103),
    tolerance = 0.001
  )
})

test_that("observed yValues are converted as expected in presence of missing values - custom units", {
  expect_equal(
    dfNAXYConvert[dfNAXYConvert$dataType == "observed", ]$yValues,
    c(0, 0.995, 0.991),
    tolerance = 0.001
  )
})
