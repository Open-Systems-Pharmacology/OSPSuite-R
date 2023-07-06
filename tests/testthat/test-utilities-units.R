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

test_that("µtants are converted to the right unicode", {
  mu1 <- rawToChar(as.raw(c(0xce, 0xbc)))
  mu2 <- rawToChar(as.raw(c(0xc2, 0xb5)))
  mu3 <- rawToChar(as.raw(0xb5))
  unit1 <- paste0(mu1, "g/", mu1, "L")
  unit2 <- paste0(mu2, "g/", mu1, "L")
  unit3 <- paste0(mu3, "g/", mu1, "L")

  expected <- paste0(ospsuiteEnv$muSymbol, "g/", ospsuiteEnv$muSymbol, "L")

  expect_identical(object = .encodeUnit(unit1), expected)
  expect_identical(object = .encodeUnit(unit2), expected)
  expect_identical(object = .encodeUnit(unit3), expected)
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
  expect_equal(getUnitsForDimension("Mass"), c("kg", "g", "mg", .encodeUnit("µg"), "ng", "pg"))
})


test_that("It can return an enum of dimensions.", {
  expect_true(.getDimensionsEnum()[["Mass"]] == "Mass")
})

test_that("It can return the expected set of units for a given dimension", {
  expect_true("kg" %in% .getUnitsEnum()[["Mass"]])
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
  expect_true(hasUnit(unit = .encodeUnit("µmol"), dimension = "Amount"))
  expect_false(hasUnit(unit = "g", "Amount"))
})

context("validateUnit")
test_that("It returns NULL when the unit exists in the dimension,
          or throws an error otherwise", {
  expect_null(validateUnit(unit = .encodeUnit("µmol"), dimension = "Amount"))
  expect_error(validateUnit(unit = "g", dimension = "Amount"), regexp = messages$errorUnitNotSupported("g", "Amount"))
})

context("getBaseUnit")
test_that("It returns the correct base unit", {
  expect_equal(getBaseUnit(dimension = "Amount"), .encodeUnit("µmol"))
})


# .unitConverter -------------------------------------------

context(".unitConverter")

# small data frame to illustrate the conversion
df <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(0.25, 45, 78),
  yUnit = c("", "%", "%"),
  yDimension = "Fraction",
  molWeight = 10
)

# also contains error columns
dfError <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(0.25, 45, 78),
  yUnit = c("", "%", "%"),
  yDimension = "Fraction",
  yErrorValues = c(0.01, 5, 8),
  yErrorUnit = c("", "%", "%"),
  molWeight = 10
)

# early return  -------------------

dfEarly <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(25, 45, 78),
  yUnit = "%",
  yDimension = "Fraction",
  molWeight = 10
)

test_that("returns early if there are only unique units and arguments are `NULL`", {
  expect_equal(dfEarly, .unitConverter(dfEarly))
})

# default conversion -------------------

dfConvert <- .unitConverter(df)

test_that("defaults for .unitConverter update xValues column as expected", {
  expect_equal(dfConvert$xValues, df$xValues)
})

test_that("defaults for .unitConverter update yValues column as expected", {
  expect_equal(dfConvert$yValues[1] / 100, df$yValues[1])
  expect_equal(dfConvert$yValues[2:3], df$yValues[2:3])
})

test_that("defaults for .unitConverter update xUnit and yUnit column as expected", {
  expect_equal(unique(dfConvert$xUnit), "min")
  expect_equal(unique(dfConvert$yUnit), "%")
})

test_that("defaults for .unitConverter leaves dimension columns untouched", {
  expect_equal(unique(dfConvert$xDimension), "Time")
  expect_equal(unique(dfConvert$yDimension), "Fraction")
})

test_that("defaults for .unitConverter leaves molWeight columns untouched", {
  expect_equal(dfConvert$molWeight, df$molWeight)
})

test_that("defaults for .unitConverter don't introduce yUnitError column if not present", {
  expect_false("yErrorUnit" %in% names(dfConvert))
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
  yUnit = "mol",
  yErrorUnit = "mol",
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
  expect_equal(unique(dfErrorConvert$yErrorUnit), "%")
  expect_equal(dfErrorConvert$yErrorValues[1] / 100, dfError$yErrorValues[1])
  expect_equal(dfErrorConvert$yErrorValues[2:3], dfError$yErrorValues[2:3])
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
  xUnit = "min",
  xDimension = "Time",
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
    dplyr::filter(dfNAConvert, dataType == "simulated")$yValues * 100,
    dplyr::filter(dfNA, dataType == "simulated")$yValues
  )
})

test_that("observed data is as expected in presence of missing values - default units", {
  # only `yValues` and unit columns should change; everything else should remain the same
  expect_equal(
    dplyr::filter(dfNAConvert, dataType == "observed")$yValues,
    dplyr::filter(dfNA, dataType == "observed")$yValues
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
    .unitConverter(dfMolWeightNA, yUnit = "mol"),
    "Molecular Weight not available."
  )
})

# missing error unit column --------------------------------

dfErrorUnitMissing <- dplyr::tibble(
  dataType = "simulated",
  xValues = c(0, 14.482, 28.965),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(25.579, 32.446, 32.103),
  yUnit = "%",
  yDimension = "Fraction",
  yErrorValues = c(2.747, 2.918, 2.746),
  molWeight = c(NA, NA, NA)
)

test_that("if yErrorUnit is missing, error values are converted correctly", {
  expect_equal(
    .unitConverter(dfErrorUnitMissing)$yErrorValues,
    dfErrorUnitMissing$yErrorValues
  )

  expect_equal(
    .unitConverter(dfErrorUnitMissing, yUnit = "")$yErrorValues,
    dfErrorUnitMissing$yErrorValues / 100
  )
})

# LLOQ column --------------------------------

dfLloq <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(0.5, 2, 3),
  yUnit = "mol",
  yErrorUnit = "mol", # error unit present without error values
  yDimension = ospDimensions$Amount,
  molWeight = 10,
  lloq = 1,
  random = "bla" # test that function doesn't remove additional columns
)

dfMWConvert <- .unitConverter(dfLloq, yUnit = ospUnits$Mass$g)

test_that("it can convert lloq columns", {
  expect_equal(dfMWConvert$lloq, rep(10, 3))
  expect_equal(unique(dfMWConvert$yUnit), ospUnits$Mass$g)
  expect_equal(unique(dfMWConvert$yErrorUnit), ospUnits$Mass$g)
})

# conversion to weeks --------------------------------

dfWeek <- dplyr::tibble(
  xValues = c(15, 0.5, 60),
  xUnit = c("min", "h", "min"),
  xDimension = "Time",
  yValues = c(0.25, 45, 78),
  yUnit = c("", "%", "%"),
  yDimension = c("Fraction", "Fraction", "Fraction"),
  yErrorValues = c(0.01, 5, 8),
  yErrorUnit = c("", "%", "%"),
  molWeight = c(10, 10, 10)
)

dfWeekConvert <- .unitConverter(dfWeek, xUnit = ospsuite::ospUnits$Time$`week(s)`)

test_that("it can convert time to weeks", {
  expect_equal(unique(dfWeekConvert$xUnit), ospsuite::ospUnits$Time$`week(s)`)
  expect_equal(unique(dfWeekConvert$xDimension), unique(dfWeek$xDimension))
  expect_equal(dfWeekConvert$xValues, c(0.001488, 0.002976, 0.005952), tolerance = 0.0001)
})

# geometric error --------------------------------

dfGeomError <- dplyr::tibble(
  xValues = c(
    0.1822785059611,
    0.425316492716471,
    0.698734219868978,
    1.0936710357666,
    1.18481000264486
  ),
  xDimension = "Time",
  xUnit = "h",
  yValues = c(
    8.91416220838437,
    13.7049000841216,
    13.5031395984697,
    12.7253497339552,
    10.3393404060625
  ),
  yErrorValues = c(
    3.8111879825592,
    2.42863011360168,
    4.66285991668701,
    4.65008020401001,
    3.5703399181366
  ),
  yDimension = "Concentration (mass)",
  yUnit = "mg/l",
  yErrorType = "GeometricStdDev",
  yErrorUnit = "",
  molWeight = 225.21
)

dfGeomErrorConvert <- .unitConverter(dfGeomError, yUnit = "µmol/l")

test_that("It shouldn't convert geometric error values or units, only `yValues`", {
  expect_equal(unique(dfGeomError$yErrorValues), unique(dfGeomErrorConvert$yErrorValues))
  expect_equal(unique(dfGeomError$yErrorUnit), unique(dfGeomErrorConvert$yErrorUnit))
  expect_equal(unique(dfGeomError$yErrorType), unique(dfGeomErrorConvert$yErrorType))

  expect_equal(unique(dfGeomErrorConvert$yUnit), "µmol/l")
  expect_equal(
    dfGeomErrorConvert$yValues,
    c(39.5815559184067, 60.8538700951183, 59.9579929775307, 56.5043725143431, 45.909774903701),
    tolerance = 0.001
  )
})

# multiple concentration dims present --------------------------------

concDims <- c(ospDimensions$`Concentration (mass)`, ospDimensions$`Concentration (molar)`)

dfConc <- dplyr::tibble(
  xValues = c(15, 0.5),
  xUnit = c("min", "h"),
  xDimension = "Time",
  yValues = c(0.25, 45),
  yUnit = c("mg/l", "mol/l"),
  yDimension = concDims,
  yErrorValues = NA,
  yErrorUnit = NA,
  molWeight = 10
)

dfConcConvert <- .unitConverter(dfConc)

test_that("it retains multiple concentration dimensions", {
  expect_equal(unique(dfConcConvert$yDimension), concDims)
})
