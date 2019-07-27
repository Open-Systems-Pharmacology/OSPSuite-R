context("Parameter")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)
liverPath <- c("Organism", "Liver")
volumePath <- c(liverPath, "Volume")

test_that("It can retrieve and update the name of a parameter", {
  par <- getParameter(c(liverPath, "Blood flow rate"), sim)
  expect_equal(par$name, "Blood flow rate")
  par$name <- "Test"
  expect_equal(par$name, "Test")
})

test_that("It can retrieve the id of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_true(par$id != "")
})

test_that("It can retrieve the path of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_equal(par$path, paste("S1", paste(volumePath, collapse = "|"), sep = "|"))
})

test_that("It throwns an error when trying to set the path of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_that(par$path <- "TOTO", throws_error())
})


test_that("It throwns an error when trying to set the id of a parameter", {
  par <- getParameter(volumePath, sim)
  expect_that(par$id <- "id", throws_error())
})

test_that("It can retrieve a value and update a valueof a parameter", {
  par <- getParameter(volumePath, sim)
  val <- par$value
  par$value <- val * 2
  expect_equal(par$value, val * 2)
})
