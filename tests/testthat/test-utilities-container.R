
context("getAllContainersMatching")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)


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
  containers <- getAllContainersMatching(c(toPathString(c("Organism", "**", "Interstitial")),
                                           toPathString(c("Organism", "Muscle", "Interstitial")),
                                           toPathString(c("Organism", "Bone", "Intracellular"))), sim)
  expect_equal(length(containers), 29)
})

test_that("It returns an empty list when no container was found", {
  containers <- getAllContainersMatching(c(toPathString(c("Organisms", "**", "Interstitial"))), sim)
  expect_equal(length(containers), 0)
  containers <- getAllContainersMatching(c(toPathString(c("Organisms", "**", "Interstitial")),
                                         toPathString(c("Organisms", "Muscle", "Interstitial"))), sim)
  expect_equal(length(containers), 0)
})

test_that("It throws an error when no valid container is provided", {
  expect_that(containers <- getAllContainersMatching(toPathString(c("Organism", "**", "Interstitial")), NULL), throws_error())
})

test_that("It throws an error when no valid path is provided", {
  expect_that(containers <- getAllContainersMatching(NULL, sim), throws_error())
})

context("getContainer")

test_that("It can retrieve a single container by path if it exists", {
  container <- getContainer(toPathString(c("Organism", "Liver", "Intracellular")), sim)
  expect_equal(container$name, "Intracellular")
})

test_that("It returns null if the  container by path does not exist", {
  container <- getContainer(toPathString(c("Organism", "Liver", "TOTO", "Length")), sim)
  expect_null(container)
})

test_that("It throws an error when trying to retrieve a container by path that would result in multiple containers", {
  expect_that(getContainer(toPathString(c("Organism", "*")), sim), throws_error())
})

test_that("It throws an error when no valid container is provided", {
  expect_that(getContainer(toPathString(c("Organism", "*")), "sim"), throws_error())
})
