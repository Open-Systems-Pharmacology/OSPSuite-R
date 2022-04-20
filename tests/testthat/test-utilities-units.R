context("toUnit")

sim <- loadTestSimulation("S1")
volumePath <- toPathString(c("Organism", "Liver", "Volume"))
par <- getParameter(volumePath, sim)

test_that("It can convert from a value in base unit to a target unit", {
  expect_equal(toUnit(par, 1, "ml"), 1000)
  expect_equal(toUnit(par, 1, "l"), 1)
})

test_that("It can convert from a value in base unit to a target unit with different case", {
  expect_equal(toUnit(par, 1, "mL"), 1000)
  expect_equal(toUnit(par, 1, "L"), 1)
})

test_that("it can convert values given as integer", {
  expect_equal(toUnit("Time", as.integer(c(60, 120, 180)), "h"), c(1, 2, 3))
})

test_that("It can convert NULL in base unit to NULL", {
  expect_null(toUnit(par, NULL, "ml"))
  expect_identical(toUnit(par, c(NULL, NULL), "ml"), c(NULL, NULL))
})

test_that("It can convert NA in base unit to NA", {
  expect_equal(toUnit(par, NA, "ml"), NA)
  expect_identical(toUnit(par, c(NA, NA), "ml"), c(NA, NA))
})


test_that("It can convert from one given value in base unit to a target unit when the molweight is defined as NA but not required", {
  expect_equal(toUnit(par, 1, "ml", molWeight = NULL), 1000)
  expect_equal(toUnit(par, 1, "l", molWeight = NA), 1)
})

test_that("It can convert from a value in base unit to a target unit using the dimension name", {
  expect_equal(toUnit(par$dimension, 1, "ml"), 1000)
})

test_that("It can convert from a value in mass to a value in mol using a molweight parameter", {
  molWeight <- 50 # 50kg/umol
  expect_equal(toUnit("Amount", 10, "kg", molWeight = molWeight), 500)
})

test_that("It can convert from an array of values in base unit to a target unit", {
  expect_equal(toUnit(par, c(1, 2, 3), "ml"), c(1000, 2000, 3000))
})

test_that("It throws an exception when converting to a unit that is not suppored", {
  expect_error(toUnit(par, 1000, "kg"))
})

test_that("It does not change the value of the quantity when converting to another unit", {
  par$value <- 5
  toUnit(par, 1, "ml")
  expect_equal(par$value, 5)
})

test_that("It can convert from a value in a non-base unit to another unit", {
  expect_equal(toUnit(quantityOrDimension = ospDimensions$Amount, values = 1, targetUnit = "mol", sourceUnit = "pmol"), 1e-12)
})

test_that("It can convert from Concentration (molar) to Concentration (mass)", {
  expect_equal(toUnit(
    quantityOrDimension = ospDimensions$`Concentration (molar)`, values = 1, targetUnit = "mg/dl", sourceUnit = "pmol/l",
    molWeight = 180, molWeightUnit = "g/mol"
  ), 1.8e-8)
})

context("toBaseUnit")
test_that("It can convert from one given value in a unit to a base unit", {
  expect_equal(toBaseUnit(par, 1000, "ml"), 1)
  expect_equal(toBaseUnit(par, 100, "l"), 100)
})

test_that("It can convert from one given value in a unit to a base unit when the molweight is defined as NA but not required", {
  expect_equal(toBaseUnit(par, 1000, "ml", molWeight = NULL), 1)
  expect_equal(toBaseUnit(par, 100, "l", molWeight = NA), 100)
})

test_that("It can convert from a value in mass to a value in mol using a molweight parameter", {
  molWeight <- 50 # 50kg/umol
  expect_equal(toBaseUnit("Amount", 500, "kg", molWeight), 10)
})

test_that("it can convert values given as integer", {
  expect_equal(toBaseUnit("Time", as.integer(c(1, 2, 3)), "h"), c(60, 120, 180))
})

test_that("It can convert from an array of values in a unit to base unit", {
  expect_equal(toBaseUnit(par, c(1000, 2000, 3000), "ml"), c(1, 2, 3))
})

test_that("It throws an exception when converting to a unit that is not suppored", {
  expect_error(toBaseUnit(par, 1000, "kg"))
})

test_that("It can convert NULL in unit to NULL", {
  expect_null(toBaseUnit(par, NULL, "ml"))
  expect_identical(toBaseUnit(par, c(NULL, NULL), "ml"), c(NULL, NULL))
})

test_that("It can convert NA in unit to NA", {
  expect_equal(toBaseUnit(par, NA, "ml"), NA)
  expect_identical(toBaseUnit(par, c(NA, NA), "ml"), c(NA, NA))
})

test_that("It can convert from an array of values with NULL entrues in a unit to base unit", {
  expect_equal(toBaseUnit(par, c(1000, 2000, 3000, NULL), "ml"), c(1, 2, 3, NULL))
})

test_that("It does not change the value of the quantity when converting to another unit", {
  par$value <- 5
  toBaseUnit(par, 1000, "ml")
  expect_equal(par$value, 5)
})

context("toDisplayUnit")

test_that("It can convert from a value in base unit to display unit", {
  expect_equal(toDisplayUnit(par, 1), 1)
})

context("allAvailableDimensions")
test_that("It should be able to return the name of all dimensions defined in the system", {
  expect_gt(length(allAvailableDimensions()), 0)
})

context("getDimensionForUnit")

test_that("It can return the expected dimension for a given unit", {
  expect_equal(getDimensionForUnit("mg"), "Mass")
})

test_that("It returns null if the dimension is not found for the unit", {
  expect_null(getDimensionForUnit("toto"))
})

context("getUnitsForDimension")

test_that("It can return the expected dimension for a given unit", {
  expect_equal(getUnitsForDimension("Mass"), c("kg", "g", "mg", "µg", "ng", "pg"))
})


test_that("It can return an enum of dimensions.", {
  expect_true(getDimensionsEnum()[["Mass"]] == "Mass")
})

test_that("It can return the expected set of units for a given dimension", {
  expect_true("kg" %in% getUnitsEnum()[["Mass"]])
})

test_that("It throws an error if the dimension is not found", {
  expect_error(getUnitsForDimension("toto"))
})

context("hasDimension")
test_that("It returns true for an existing dimension, false otherwise", {
  expect_true(hasDimension("Amount"))
  expect_false(hasDimension("AAmount"))
})

context("validateDimension")
test_that("It returns NULL when the dimension exists,
          or throws an error otherwise", {
  expect_null(validateDimension("Amount"))
  expect_error(validateDimension("AAmount"), regexp = messages$errorDimensionNotSupported("AAmount"))
})

context("hasUnit")
test_that("It returns true for an existing unit in the dimension, false otherwise", {
  expect_true(hasUnit(unit = "µmol", dimension = "Amount"))
  expect_false(hasUnit(unit = "g", "Amount"))
})

context("validateUnit")
test_that("It returns NULL when the unit exists in the dimension,
          or throws an error otherwise", {
  expect_null(validateUnit(unit = "µmol", dimension = "Amount"))
  expect_error(validateUnit(unit = "g", dimension = "Amount"), regexp = messages$errorUnitNotSupported("g", "Amount"))
})

context("getBaseUnit")
test_that("It returns the correct base unit", {
  expect_equal(getBaseUnit(dimension = "Amount"), "µmol")
})


context(".unitConverter")

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

test_that("the order of rows is not changed", {
  expect_equal(dfNA$dataType, dfNAConvert$dataType)
  expect_equal(dfNA$dataType, dfNAXYConvert$dataType)
})

test_that("the order of columns is not changed", {
  expect_equal(names(dfNA), names(dfNAConvert))
  expect_equal(names(dfNA), names(dfNAXYConvert))
})

test_that("simulated data is as expected in presence of missing values - default units", {
  # because of the missing values, no conversion should have taken place and so these two
  # data frames should be identical
  expect_equal(
    dplyr::filter(dfNAConvert, dataType == "simulated"),
    dplyr::filter(dfNA, dataType == "simulated")
  )
})

test_that("observed data is as expected in presence of missing values - default units", {
  # only `yValues` and unit columns should change; everything else should remain the same
  expect_equal(
    dplyr::filter(dfNAConvert, dataType == "observed"),
    dplyr::filter(dfNA, dataType == "observed") %>%
      dplyr::mutate(yValues = 100 * yValues, yUnit = "%", yErrorUnit = "%")
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

# missing molecular weight -------------------

dfMolWeightNA <- dplyr::tibble(
  dataType = c(rep("simulated", 3), rep("observed", 3)),
  xValues = c(0, 14.482, 28.965, 0, 1, 2),
  xUnit = "min", xDimension = "Time",
  yValues = c(1, 1, 1, 1, 1, 1),
  yUnit = c("mol", "mol", "mol", "g", "g", "g"),
  yDimension = c("Amount", "Amount", "Amount", "Mass", "Mass", "Mass"),
  yErrorValues = c(2.747, 2.918, 2.746, NA, NA, NA),
  yErrorUnit = c("mol", "mol", "mol", "g", "g", "g"),
  molWeight = c(10, 10, 20, 20, NA, 10)
)

test_that("if molWeight is missing, an error is signaled if dimensions require them", {
  expect_error(
    .unitConverter(dfMolWeightNA),
    "Molecular Weight not available."
  )
})
