context("Parameter")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)
liverPathArray <- c("Organism", "Liver")
liverPath <- toPathString(liverPathArray)
volumePath <- toPathString(c(liverPathArray, "Volume"))

test_that("It can retrieve name of a parameter", {
  par <- getParameter(toPathString(c(liverPathArray, "Blood flow rate")), sim)
  expect_equal(par$name, "Blood flow rate")
})

test_that("It throwns an error when trying to set the name of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_that(par$name <- "TOTO", throws_error())
})

test_that("It can retrieve the id of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_false(is.null(par$id))
})

test_that("It can retrieve the path of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_equal(par$path, paste("S1", paste(volumePath, collapse = "|"), sep = "|"))
})

test_that("It can update the persistable state of a parameter", {
  par <- getParameter(volumePath, sim)
  par$persistable <- TRUE
  expect_true(par$persistable)
  par$persistable <- FALSE
  expect_false(par$persistable)
})


test_that("It throwns an error when trying to set the path of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_that(par$path <- "TOTO", throws_error())
})


test_that("It throwns an error when trying to set the id of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_that(par$id <- "id", throws_error())
})

test_that("It can retrieve a value and update a value of a parameter", {
  par <- getParameter(volumePath, sim)
  val <- par$value
  par$value <- val * 2
  expect_equal(par$value, val * 2)
})

test_that("It can retrieve the dimension of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_equal(par$dimension, "Volume")
})

test_that("It can retrieve the unit of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_equal(par$unit, "l")
})
