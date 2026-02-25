# Container

sim <- loadTestSimulation("S1")
liverPath <- toPathString(c("Organism", "Liver"))

test_that("Printing of the container", {
  container <- getContainer(liverPath, sim)
  expectSnapshotPrint(container)
})

test_that("It can retrieve the name of a container", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$name, "Liver")
})

test_that("It can retrieve the id of a container", {
  container <- getContainer(liverPath, sim)
  expect_false(is.null(container$id))
})

test_that("It can retrieve the type of a container", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$containerType, "Organ")
})

test_that("It can retrieve the path of a container", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$path, liverPath)
})

test_that("It can retrieve the fullPath of a container", {
  container <- getContainer(liverPath, sim)
  expect_equal(container$fullPath, paste("S1", liverPath, sep = "|"))
})

test_that("It throws an error when trying to set the path of a container", {
  container <- getContainer(liverPath, sim)
  expectPropertyReadOnly(container, "path", "TOTO")
})


test_that("It throws an error when trying to set the id of a container", {
  container <- getContainer(liverPath, sim)
  expectPropertyReadOnly(container, "id", "id")
})


test_that("It throws an error when trying to set the name of a container", {
  container <- getContainer(liverPath, sim)
  expectPropertyReadOnly(container, "name", "name")
})

test_that("It can print container", {
  container <- getContainer(liverPath, sim)
  expectSnapshotPrint(container)
})
