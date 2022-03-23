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
  expect_that(toUnit(par, 1000, "kg"), throws_error())
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
  expect_that(toBaseUnit(par, 1000, "kg"), throws_error())
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
  expect_that(getUnitsForDimension("toto"), throws_error())
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
