context("unitConverter")

# small dataframe to illustrate the conversion
df <- dplyr::tibble(
  xValues = c(15, 30, 60), xUnit = "min", xDimension = "Time",
  yValues = c(0.25, 45, 78), yUnit = c("", "%", "%"), yDimension = c("Fraction", "Fraction", "Fraction"),
  molWeight = c(12.5, 10, 5)
)

# validation input -------------------

test_that("unitConverter fails with incorrect data arguments", {
  expect_error(unitConverter(as.matrix(table(mtcars$cyl))), messages$errorWrongType("data", "matrix", "data.frame"))
})

test_that("unitConverter fails with incorrect unit arguments", {
  expect_error(unitConverter(df, xUnit = 5), messages$errorWrongType("xUnit", "numeric", "character"))
  expect_error(unitConverter(df, yUnit = 5), messages$errorWrongType("yUnit", "numeric", "character"))
})

# default conversion -------------------

dfConvert <- unitConverter(df)

test_that("defaults for unitConverter update xValues column as expected", {
  expect_equal(dfConvert$xValues, df$xValues)
})

test_that("defaults for unitConverter update yValues column as expected", {
  expect_equal(dfConvert$yValues[1], df$yValues[1])
  expect_equal(dfConvert$yValues[2:3] * 100, df$yValues[2:3])
})

test_that("defaults for unitConverter update xUnit and yUnit column as expected", {
  expect_equal(unique(dfConvert$xUnit), unique(dfConvert$xUnit))
  expect_equal(unique(dfConvert$yUnit), unique(dfConvert$yUnit)[1])

  expect_equal(unique(dfConvert$xUnit), "min")
  expect_equal(unique(dfConvert$yUnit), "")
})

test_that("defaults for unitConverter leaves dimension columns untouched", {
  expect_equal(unique(dfConvert$xDimension), "Time")
  expect_equal(unique(dfConvert$yDimension), "Fraction")
})

# only `xUnit` -------------------

dfXConvert <- unitConverter(df, xUnit = ospUnits$Time$h)

test_that("unitConverter converts xValues column as expected", {
  expect_equal(dfXConvert$xValues, df$xValues / 60) # 1 hour = 60 minutes
})

test_that("unitConverter updates xUnit column as expected", {
  expect_equal(unique(dfXConvert$xUnit), ospUnits$Time$h)
})

# only `yUnit` -------------------

dfYConvert <- unitConverter(df, yUnit = ospUnits$Fraction$`%`)

test_that("unitConverter converts yValues as expected", {
  expect_equal(dfYConvert$yValues[1], df$yValues[1] * 100)
  expect_equal(dfYConvert$yValues[2:3], df$yValues[2:3])
})

test_that("unitConverter updates yUnit column as expected", {
  expect_equal(unique(dfYConvert$yUnit), ospUnits$Fraction$`%`)
})

# both `xUnit` and `yUnit` -------------------

dfXYConvert <- unitConverter(df, xUnit = ospUnits$Time$h, yUnit = ospUnits$Fraction$`%`)

test_that("unitConverter converts both xValues and yValues column as expected", {
  expect_equal(dfXYConvert$xValues, df$xValues / 60)
  expect_equal(dfXYConvert$yValues[1], df$yValues[1] * 100)
  expect_equal(dfXYConvert$yValues[2:3], df$yValues[2:3])
})

test_that("unitConverter updates xUnit and yUnit column as expected", {
  expect_equal(unique(dfXYConvert$xUnit), ospUnits$Time$h)
  expect_equal(unique(dfXYConvert$yUnit), ospUnits$Fraction$`%`)
})
