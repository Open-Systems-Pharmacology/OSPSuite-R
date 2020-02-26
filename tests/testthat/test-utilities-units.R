context("toUnit")

sim <- loadTestSimulation("S1")
volumePath <- toPathString(c("Organism", "Liver", "Volume"))
par <- getParameter(volumePath, sim)

test_that("It can convert from a value in base unit to a target unit", {
  expect_equal(toUnit(par, 1, "ml"), 1000)
  expect_equal(toUnit(par, 1, "l"), 1)
})

test_that("It can convert from a value in base unit to a target unit using the dimension name", {
  expect_equal(toUnit(par$dimension, 1, "ml"), 1000)
})

test_that("It can convert from a value in mass to a value in mol using a molweight parameter", {
  molWeight <- 50 #50umol/kg
  expect_equal(toUnit("Amount", 10, "kg", molWeight), 500)
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


context("toBaseUnit")
test_that("It can convert from one given value in a unit to a base unit", {
  expect_equal(toBaseUnit(par, 1000, "ml"), 1)
  expect_equal(toBaseUnit(par, 100, "l"), 100)
})

test_that("It can convert from an array of values in a unit to base unit", {
  expect_equal(toBaseUnit(par, c(1000, 2000, 3000), "ml"), c(1, 2, 3))
})

test_that("It throws an exception when converting to a unit that is not suppored", {
  expect_that(toBaseUnit(par, 1000, "kg"), throws_error())
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

