
context("getAllParametersMatching")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)


test_that("It can retrieve parameters with absolute path", {
  parameters <- getAllParametersMatching(c("Organism", "Liver", "Intracellular", "Volume"), sim)
  expect_equal(length(parameters), 1)
})

test_that("It can retrieve parameters with generic path path", {
  parameters <- getAllParametersMatching(c("Organism", "Liv*", "Intracellu*", "Vol*"), sim)
  expect_equal(length(parameters), 1)
})

test_that("It can retrieve all parameters matching a given criteria with wild card entry", {
  parameters <- getAllParametersMatching(c("Organism", "Liver", "*", "Volume"), sim)
  expect_equal(length(parameters), 6) # 6 sub compartments in liver
})

test_that("It can retrieve all parameters matching a given criteria with generic path entry", {
  parameters <- getAllParametersMatching(c("Organism", "Muscle", "**", "Volume"), sim)
  expect_equal(length(parameters), 5) # 4 compartments + own volume
})


context("getParameter")

test_that("It can retrieve a single parameter by path if it exists", {
  parameter <- getParameter(c("Organism", "Liver", "Intracellular", "Volume"), sim)
  expect_equal(parameter$name, "Volume")
})

test_that("It returns null if the  parameter by path does not exist", {
  parameter <- getParameter(c("Organism", "Liver", "Intracellular", "Length"), sim)
  expect_null(parameter)
})

test_that("It throwns an error when trying to retrieve a parameter by path that would result in multiple parameters", {
  expect_that(getParameter(c("Organism", "Liver", "*"), sim), throws_error())
})
