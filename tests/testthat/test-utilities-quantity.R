
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
