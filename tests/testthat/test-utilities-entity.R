
context("uniqueEntity")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)


test_that("It throws an error when no valid entities are provided", {
  expect_that(uniqueEntity("String"), throws_error())
})

test_that("It throws an error when no valid 'compareBy' is provided", {
  parameter <- getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim)
  expect_that(uniqueEntity(parameter, compareBy = 2), throws_error())
  expect_that(uniqueEntity(parameter, compareBy = "2"), throws_error())
})

test_that("It can filter by id", {
  parameters <- c(getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim),
                  getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim),
                  getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")), sim))
  expect_equal(length(uniqueEntity(parameters)), 2)
})

test_that("It can filter by name", {
  parameters <- c(getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim),
                  getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim),
                  getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")), sim))
  expect_equal(length(uniqueEntity(parameters, compareBy = "name")), 2)
})

test_that("It can filter by path", {
  parameters <- c(getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim),
                  getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Volume")), sim),
                  getParameter(toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")), sim))
  expect_equal(length(uniqueEntity(parameters, compareBy = "path")), 2)
})
