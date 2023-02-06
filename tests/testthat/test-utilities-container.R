context("getAllContainersMatching")

sim <- loadTestSimulation("S1")

test_that("It can retrieve containers with absolute path", {
  containers <- getAllContainersMatching(toPathString(c("Organism", "Liver", "Intracellular")), sim)
  expect_equal(length(containers), 1)
})

test_that("It can retrieve containers with generic path path", {
  containers <- getAllContainersMatching(toPathString(c("Organism", "*", "Intracellu*")), sim)
  expect_equal(length(containers), 15)
})

test_that("It can retrieve all containers matching a given criteria with wild card entry", {
  containers <- getAllContainersMatching(toPathString(c("Organism", "Liver", "Pericentral", "*")), sim)
  expect_equal(length(containers), 5) # 4 sub compartments in liver pericentral + drug
})

test_that("It can retrieve all containers matching a given criteria with generic path entry", {
  containers <- getAllContainersMatching(toPathString(c("Organism", "**", "Interstitial")), sim)
  expect_equal(length(containers), 28)
})

test_that("It can retrieve containers from multiple paths", {
  containers <- getAllContainersMatching(c(
    toPathString(c("Organism", "**", "Interstitial")),
    toPathString(c("Organism", "Muscle", "Interstitial")),
    toPathString(c("Organism", "Bone", "Intracellular"))
  ), sim)
  expect_equal(length(containers), 29)
})

test_that("It returns an empty list when no container was found", {
  containers <- getAllContainersMatching(c(toPathString(c("Organisms", "**", "Interstitial"))), sim)
  expect_equal(length(containers), 0)
  containers <- getAllContainersMatching(c(
    toPathString(c("Organisms", "**", "Interstitial")),
    toPathString(c("Organisms", "Muscle", "Interstitial"))
  ), sim)
  expect_equal(length(containers), 0)
})

test_that("It throws an error when no valid container is provided", {
  expect_error(containers <- getAllContainersMatching(toPathString(c("Organism", "**", "Interstitial")), NULL))
})

test_that("It throws an error when no valid path is provided", {
  expect_error(containers <- getAllContainersMatching(NULL, sim))
})

context("getAllContainerPathsIn")

test_that("It can retrieve all container paths defined in the simulation", {
  paths <- getAllContainerPathsIn(sim)
  expect_gt(length(paths), 0)
})

test_that("It can retrieve all container paths defined in a container", {
  paths <- getAllContainerPathsIn(sim$root)
  expect_gt(length(paths), 0)
})

context("getContainer")

test_that("It can retrieve a single container by path if it exists", {
  container <- getContainer(toPathString(c("Organism", "Liver", "Intracellular")), sim)
  expect_equal(container$name, "Intracellular")
})

test_that("It returns null if the  container by path does not exist and stopIfNotFound == FALSE", {
  container <- getContainer(toPathString(c("Organism", "Liver", "TOTO", "Length")), sim, stopIfNotFound = FALSE)
  expect_null(container)
})

test_that("It throws an error if the container by path does not exist", {
  expect_error((container <- getContainer(toPathString(c("Organism", "Liver", "TOTO", "Length")), sim)))
})

test_that("It throws an error when trying to retrieve a container by path that would result in multiple containers", {
  expect_error(getContainer(toPathString(c("Organism", "*")), sim))
})

test_that("It throws an error when no valid container is provided", {
  expect_error(getContainer(toPathString(c("Organism", "*")), "sim"))
})
