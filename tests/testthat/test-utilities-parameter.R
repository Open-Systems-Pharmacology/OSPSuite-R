
context("getAllParametersMatching")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)


test_that("It can retrieve parameters with absolute path", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(length(parameters), 1)
})

test_that("It can retrieve parameters with generic path path", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liv*", "Intracellu*", "Vol*")), sim)
  expect_equal(length(parameters), 1)
})

test_that("It can retrieve all parameters matching a given criteria with wild card entry", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim)
  expect_equal(length(parameters), 6) # 6 sub compartments in liver
})

test_that("It can retrieve all parameters matching a given criteria with generic path entry", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Muscle", "**", "Volume")), sim)
  expect_equal(length(parameters), 5) # 4 compartments + own volume
})

test_that("It throws an error when no valid container is provided", {
  expect_that(parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "Intracellular", "Volume"), NULL)), throws_error())
})

test_that("It throws an error when no valid path is provided", {
  expect_that(parameters <- getAllParametersMatching(NULL, sim), throws_error())
})

context("getParameter")

test_that("It can retrieve a single parameter by path if it exists", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(parameter$name, "Volume")
})

test_that("It returns null if the  parameter by path does not exist", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Length")), sim)
  expect_null(parameter)
})

test_that("It throws an error when trying to retrieve a parameter by path that would result in multiple parameters", {
  expect_that(getParameter(toPathString(c("Organism", "Liver", "*")), sim), throws_error())
})

context("setParametersValues")

test_that("It throws an error when no valid parameter objects are provided", {
  expect_that(setParametersValues("parameter", 1), throws_error())
  parameters <- c(getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim), "1")
  expect_that(setParametersValues(parameters, 1), throws_error())
})

test_that("It throws an error when no valid values are provided", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_that(setParametersValues(parameter, "s"), throws_error())
})

test_that("It throws an error when the number of parameters differs from the number of values", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim)
  expect_that(setParametersValues(parameter, c(1, 2)), throws_error())
  expect_that(setParametersValues(parameters, c(1:5)), throws_error())
})

test_that("It can set the value of a single parameter", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  setParametersValues(parameter, 1)
  expect_equal(parameter$value, 1)
})

test_that("It can set the values of multiple parameters", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim)
  setParametersValues(parameters, c(1:6))
  newVals <- sapply(parameters, fun <- function(x) {
    x$value
  })
  expect_equal(newVals, c(1:6))
})
