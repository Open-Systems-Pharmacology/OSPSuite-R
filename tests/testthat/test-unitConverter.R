context(".unitConverter")

# data -------------------

# small data frame to illustrate the conversion
df <- dplyr::tibble(
  xValues = c(15, 30, 60), xUnit = "min", xDimension = "Time",
  yValues = c(0.25, 45, 78), yUnit = c("", "%", "%"), yDimension = c("Fraction", "Fraction", "Fraction"),
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
