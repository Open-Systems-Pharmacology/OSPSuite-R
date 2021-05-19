context("getAllQuantitiesMatching")

sim <- loadTestSimulation("S1")

test_that("It can retrieve quantities with absolute path", {
  quantities <- getAllQuantitiesMatching(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(length(quantities), 1)
})

test_that("It can retrieve quantities with generic path path", {
  quantities <- getAllQuantitiesMatching(toPathString(c("Organism", "Liv*", "Intracellu*", "Vol*")), sim)
  expect_equal(length(quantities), 1)
})


context("getAllQuantityPathsIn")

test_that("It can retrieve all quantity paths defined in the simulation", {
  paths <- getAllQuantityPathsIn(sim)
  expect_gt(length(paths), 0)
})

test_that("It can retrieve all quantity paths defined in a container", {
  paths <- getAllQuantityPathsIn(sim$root)
  expect_gt(length(paths), 0)
})


context("getQuantity")

test_that("It can retrieve a single quantity by path if it exists", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(quantity$name, "Volume")
})

test_that("It throws an error if the quantity by path does not exist", {
  expect_error((quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Length")), sim)))
})

test_that("It returns null if the quantity by path does not exist and stopIfNotFound == FALSE", {
  quantity <- getQuantity(toPathString(c("Organism", "Liver", "Intracellular", "Length")), sim, stopIfNotFound = FALSE)
  expect_null(quantity)
})

test_that("It throws an error when trying to retrieve a quantity by path that would result in multiple quantities", {
  expect_that(getQuantity(toPathString(c("Organism", "Liver", "*")), sim), throws_error())
})


context("setQuantityValuesByPath")

test_that("It can set single parameter values", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  setQuantityValuesByPath(parameterPath, 100, sim)
  parameter <- getParameter(parameterPath, sim)
  expect_equal(parameter$value, 100)
})

test_that("It can set multiple quantity values", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  setQuantityValuesByPath(c(quantityPath1, quantityPath2), c(40, 50), sim)
  quantity1 <- getQuantity(quantityPath1, sim)
  quantity2 <- getQuantity(quantityPath2, sim)
  expect_equal(quantity1$value, 40)
  expect_equal(quantity2$value, 50)
})

test_that("It throws an exception when setting values for a quantity that does not exist", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath <- "Organism|Liver|NOPE|Volume"
  expect_that(setQuantityValuesByPath(parameterPath, 100, sim), throws_error())
})
