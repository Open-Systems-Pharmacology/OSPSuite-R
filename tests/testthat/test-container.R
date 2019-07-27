context("Container")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)
liverPath <- c("Organism", "Liver")

test_that("It can update the name of a container", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$name, "Liver")
})

test_that("It can retrieve the id of a container", {
  container <- getContainer(liverPath, sim)
  expect_true(container$id != "")
})

test_that("It can retrieve the type of a container", {
  container <- getContainer(liverPath, sim)
  print(container$containerType)
  expect_equal(container$containerType, ContainerType$Organ)
})

test_that("It can retrieve the path of a parameter", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$path, paste("S1", paste(liverPath, collapse = "|"), sep = "|"))
})

test_that("It throwns an error when trying to set the path of a container", {
  container <- getContainer(liverPath, sim)
  expect_that(container$path <- "TOTO", throws_error())
})


test_that("It throwns an error when trying to set the id of a container", {
  container <- getContainer(liverPath, sim)
  expect_that(container$id <- "id", throws_error())
})
