
context("getAllParametersMatching")

sim <- loadTestSimulation("S1")

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

test_that("It can retrieve parameters from multiple paths", {
  parameters <- getAllParametersMatching(c(
    toPathString(c("Organism", "Muscle", "**", "Volume")),
    toPathString(c("Organism", "Muscle", "Intracellular", "Volume")),
    toPathString(c("Organism", "Bone", "Intracellular", "Volume"))
  ), sim)
  expect_equal(length(parameters), 6) # 4 compartments + own volume + volume of bone intracellular
})

test_that("It returns an empty list when no parameter was found", {
  parameters <- getAllParametersMatching(c(toPathString(c("Organism", "Muscles", "**", "Volume"))), sim)
  expect_equal(length(parameters), 0)
  parameters <- getAllParametersMatching(c(
    toPathString(c("Organism", "Muscles", "**", "Volume")),
    toPathString(c("Organism", "Muscles", "Intracellular", "Volume"))
  ), sim)
  expect_equal(length(parameters), 0)
})

test_that("It throws an error when no valid container is provided", {
  expect_that(parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), NULL), throws_error())
})

test_that("It throws an error when no valid path is provided", {
  expect_that(parameters <- getAllParametersMatching(NULL, sim), throws_error())
})

context("getAllParameterPathsIn")

test_that("It can retrieve all parameter paths defined in the simulation", {
  paths <- getAllParameterPathsIn(sim)
  expect_gt(length(paths), 0)
})

test_that("It can retrieve all parameter paths defined in a container", {
  paths <- getAllParameterPathsIn(sim$root)
  expect_gt(length(paths), 0)
})

context("getParameter")

test_that("It can retrieve a single parameter by path if it exists", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_equal(parameter$name, "Volume")
})

test_that("It returns null if the  parameter by path does not exist and stopIfNotFound == FALSE", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Length")), sim, stopIfNotFound = FALSE)
  expect_null(parameter)
})

test_that("It throws an error if the parameter by path does not exist", {
  expect_error((parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Length")), sim)))
})

test_that("It throws an error when trying to retrieve a parameter by path that would result in multiple parameters", {
  expect_that(getParameter(toPathString(c("Organism", "Liver", "*")), sim), throws_error())
})

context("setParameterValues")

test_that("It throws an error when no valid parameter objects are provided", {
  expect_that(setParameterValues("parameter", 1), throws_error())
  parameters <- c(getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim), "1")
  expect_that(setParameterValues(parameters, 1), throws_error())
})

test_that("It throws an error when no valid values are provided", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  expect_that(setParameterValues(parameter, "s"), throws_error())
})

test_that("It throws an error when the number of parameters differs from the number of values", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim)
  expect_that(setParameterValues(parameter, c(1, 2)), throws_error())
  expect_that(setParameterValues(parameters, c(1:5)), throws_error())
})

test_that("It can set the value of a single parameter", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  setParameterValues(parameter, 1)
  expect_equal(parameter$value, 1)
})

test_that("It can set the values of multiple parameters", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim)
  setParameterValues(parameters, c(1:6))
  newVals <- sapply(parameters, function(x) {
    x$value
  })
  expect_equal(newVals, c(1:6))
})


context("setParameterValuesByPath")

test_that("It can set single parameter values", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath <- "Organism|Liver|Intracellular|Volume"
  setParameterValuesByPath(parameterPath, 100, sim)
  parameter <- getParameter(parameterPath, sim)
  expect_equal(parameter$value, 100)
})

test_that("It can set multiple parameter values", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath1 <- "Organism|Liver|Intracellular|Volume"
  parameterPath2 <- "Organism|Kidney|Intracellular|Volume"
  setParameterValuesByPath(c(parameterPath1, parameterPath2), c(40, 50), sim)
  parameter1 <- getParameter(parameterPath1, sim)
  parameter2 <- getParameter(parameterPath2, sim)
  expect_equal(parameter1$value, 40)
  expect_equal(parameter2$value, 50)
})

test_that("It throws an exception when setting values for a parameter that does not exist", {
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  parameterPath1 <- "Organism|Liver|NOPE|Volume"
  expect_that(setParameterValuesByPath(parameterPath, 100, sim), throws_error())
})

test_that("It can get the value of an individual from a population and set them into a simulation", {
  populationFileName <- getTestDataFilePath("pop_10.csv")
  population <- loadPopulation(populationFileName)
  sim <- loadTestSimulation("S1", loadFromCache = TRUE)
  individualValues <- population$getParameterValuesForIndividual(8)
  setParameterValuesByPath(individualValues$paths, individualValues$values, sim)
  parameter <- getParameter(individualValues$paths[1], sim)
  expect_equal(parameter$value, individualValues$values[1])
})

context("scaleParameterValues")

test_that("It can scale a single parameter with a given factor", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Intracellular", "Volume")), sim)
  originalValue <- parameter$value
  scaleParameterValues(parameter, 1.5)
  expect_equal(parameter$value, originalValue * 1.5)
})

test_that("It can scale mulstiple parameters with a given factor", {
  parameters <- getAllParametersMatching(toPathString(c("Organism", "Liver", "*", "Volume")), sim)
  originalValues <- sapply(parameters, function(x) x$value)
  scaleParameterValues(parameters, 1.5)
  scaledValues <- sapply(parameters, function(x) x$value)
  expect_equal(scaledValues, originalValues * 1.5)
})
