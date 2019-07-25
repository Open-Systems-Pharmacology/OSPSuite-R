
context("loadSimulation")

# TODO. Not sure why this is required
library(rClr)

initPackage()
dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)


test_that("It can load a valid pkml simulation file", {
  simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)

  sim <- loadSimulation(simFile)


  expect_true(!is.null(sim))
})

test_that("It throws an exception of the pkml loaded is not a valid simulation file", {
  simFile <- file.path(dataPath, "molecules.pkml", fsep = .Platform$file.sep)

  expect_that( loadSimulation(simFile), throws_error("Could not load simulation"))
})

