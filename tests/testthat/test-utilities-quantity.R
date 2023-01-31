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


context("getAllObserverPathsIn")

test_that("It can retrieve all quantity paths defined in the simulation", {
  paths <- getAllObserverPathsIn(sim)
  expect_gt(length(paths), 0)
})

test_that("It can retrieve all quantity paths defined in a container", {
  paths <- getAllObserverPathsIn(sim$root)
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
  expect_error(getQuantity(toPathString(c("Organism", "Liver", "*")), sim))
})


context("setQuantityValuesByPath")
sim <- loadTestSimulation("S1", loadFromCache = TRUE)

test_that("It can set single parameter value", {
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  setQuantityValuesByPath(parameterPath, 100, sim)
  parameter <- getParameter(parameterPath, sim)
  expect_equal(parameter$value, 100)
})

test_that("It can set single parameter value with unit", {
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  setQuantityValuesByPath(quantityPaths = parameterPath, values = 100, simulation = sim, units = "ml")
  parameter <- getParameter(parameterPath, sim)
  expect_equal(parameter$value, 100e-3)
})

test_that("It can set multiple quantity values", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  setQuantityValuesByPath(c(quantityPath1, quantityPath2), c(40, 50), sim)
  quantity1 <- getQuantity(quantityPath1, sim)
  quantity2 <- getQuantity(quantityPath2, sim)
  expect_equal(quantity1$value, 40)
  expect_equal(quantity2$value, 50)
})

test_that("It can set multiple quantity values with units", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  setQuantityValuesByPath(
    quantityPaths = c(quantityPath1, quantityPath2), values = c(40, 50), simulation = sim,
    units = c("ml", "mol")
  )
  quantity1 <- getQuantity(quantityPath1, sim)
  quantity2 <- getQuantity(quantityPath2, sim)
  expect_equal(quantity1$value, 40e-3)
  expect_equal(quantity2$value, 50e6)
})

test_that("It throws an exception when setting values for a quantity that does not exist", {
  parameterPath <- "Organism|Liver|NOPE|Volume"
  expect_error(setQuantityValuesByPath(parameterPath, 100, sim))
})

test_that("It does not throw an exception when setting values for a quantity that does not exist with unit", {
  parameterPath <- "Organism|Liver|NOPE|Volume"
  expect_error(setQuantityValuesByPath(quantityPaths = parameterPath, values = 100, simulation = sim, units = "ml", stopIfNotFound = FALSE), NA)
})

test_that("It does not throw an exception when setting values for a quantity that does not exist and the stopIfnotFound flag is set to false", {
  parameterPath <- "Organism|Liver|NOPE|Volume"
  expect_error(setQuantityValuesByPath(quantityPaths = parameterPath, values = 100, simulation = sim, stopIfNotFound = FALSE), NA)
})

test_that("It throws an error when the number of quantity paths differs from the number units", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  expect_error(setQuantityValuesByPath(c(quantityPath1, quantityPath2), c(40, 50), sim, units = "ml"))
})

context("getQuantityValuesByPath")
test_that("It can get single parameter value", {
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  value <- getQuantityValuesByPath(parameterPath, sim)
  parameter <- getParameter(parameterPath, sim)
  expect_equal(parameter$value, value)
})

test_that("It can get single parameter value with unit", {
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  value <- getQuantityValuesByPath(quantityPaths = parameterPath, simulation = sim, units = "ml")
  parameter <- getParameter(parameterPath, sim)
  paramValue <- toUnit(
    quantityOrDimension = parameter,
    values = parameter$value,
    targetUnit = "ml"
  )
  expect_equal(paramValue, value)
})

test_that("It can get multiple quantity values", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  values <- getQuantityValuesByPath(
    quantityPaths = list(quantityPath1, quantityPath2),
    simulation = sim
  )
  quantity1 <- getQuantity(quantityPath1, sim)
  quantity2 <- getQuantity(quantityPath2, sim)
  expect_equal(quantity1$value, values[[1]])
  expect_equal(quantity2$value, values[[2]])
})

test_that("It can get multiple quantity values with units", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  values <- getQuantityValuesByPath(
    quantityPaths = list(quantityPath1, quantityPath2), simulation = sim,
    units = list("ml", "mol")
  )
  quantity <- getQuantity(quantityPath1, sim)
  quantityValue <- toUnit(
    quantityOrDimension = quantity,
    values = quantity$value,
    targetUnit = "ml"
  )
  expect_equal(quantityValue, values[[1]])
  quantity <- getQuantity(quantityPath2, sim)
  quantityValue <- toUnit(
    quantityOrDimension = quantity,
    values = quantity$value,
    targetUnit = "mol"
  )
  expect_equal(quantityValue, values[[2]])
})

test_that("It can get multiple quantity values with units when one unit is NULL", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  values <- getQuantityValuesByPath(
    quantityPaths = list(quantityPath1, quantityPath2), simulation = sim,
    units = list("ml", NULL)
  )
  quantity <- getQuantity(quantityPath1, sim)
  quantityValue <- toUnit(
    quantityOrDimension = quantity,
    values = quantity$value,
    targetUnit = "ml"
  )
  expect_equal(quantityValue, values[[1]])
  quantity <- getQuantity(quantityPath2, sim)
  quantityValue <- quantity$value
  expect_equal(quantityValue, values[[2]])
})

test_that("It throws an exception when getting values for a quantity that does not exist", {
  parameterPath <- "Organism|Liver|NOPE|Volume"
  expect_error(getQuantityValuesByPath(parameterPath, sim))
})

test_that("It does not throw an exception when getting values for a quantity that does not exist with unit and the stopIfnotFound flag is set to fals", {
  parameterPath <- "Organism|Liver|NOPE|Volume"
  expect_error(getQuantityValuesByPath(quantityPaths = parameterPath, simulation = sim, units = "ml", stopIfNotFound = FALSE), NA)
})

test_that("It throws an error when the number of quantity paths differs from the number units", {
  quantityPath1 <- "Organism|Liver|Intracellular|Volume"
  quantityPath2 <- "Organism|VenousBlood|Plasma|CYP3A4"
  expect_error(getQuantityValuesByPath(c(quantityPath1, quantityPath2), sim, units = "ml"))
})


# isExplicitFormulaByPath
nonFormulaPath <- "Organism|Liver|Volume"
formulaPath <- "Organism|Weight"

test_that("It throws an error when a quantity with the given path is not found and stopIfNotFound is TRUE", {
  expect_error(isExplicitFormulaByPath(
    path = "foo",
    simulation = sim
  ))
})

test_that("It returns FALSE when a quantity with the given path is not found and stopIfNotFound is FALSE", {
  expect_false(isExplicitFormulaByPath(
    path = "foo",
    simulation = sim,
    stopIfNotFound = FALSE
  ))
})

test_that("It returns FALSE when the quantity is not an explicit formula", {
  expect_false(isExplicitFormulaByPath(
    path = nonFormulaPath,
    simulation = sim
  ))
})

test_that("It returns TRUE when the quantity is an explicit formula", {
  expect_true(isExplicitFormulaByPath(
    path = formulaPath,
    simulation = sim
  ))
})
