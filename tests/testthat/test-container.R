context("Container")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)
liverPath <- toPathString(c("Organism", "Liver"))

test_that("It can retrieve the name of a container", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$name, "Liver")
})

test_that("It can retrieve the id of a container", {
  container <- getContainer(liverPath, sim)
  expect_false(is.null(container$id))
})

test_that("It can retrieve the type of a container as int", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$containerTypeInt, ContainerType$Organ)
})

test_that("It can retrieve the type of a container as a string", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$containerTypeStr, "Organ")
})

test_that("It can retrieve the path of a parameter", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$path, paste("S1", liverPath, sep = "|"))
})

test_that("It throws an error when trying to set the path of a container", {
  container <- getContainer(liverPath, sim)
  expect_that(container$path <- "TOTO", throws_error())
})


test_that("It throws an error when trying to set the id of a container", {
  container <- getContainer(liverPath, sim)
  expect_that(container$id <- "id", throws_error())
})


test_that("It throws an error when trying to set the name of a container", {
  container <- getContainer(liverPath, sim)
  expect_that(container$name <- "name", throws_error())
})
