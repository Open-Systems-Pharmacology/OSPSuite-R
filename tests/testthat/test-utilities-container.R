
context("getContainers")

dataPath <- file.path(getwd(), "..", "data", fsep = .Platform$file.sep)
simFile <- file.path(dataPath, "S1.pkml", fsep = .Platform$file.sep)
sim <- loadSimulation(simFile)


test_that("It can retrieve containers with absolute path", {
  containers <- getContainers(c("Organism", "Liver", "Intracellular"), sim)
  expect_equal(length(containers), 1)
})

test_that("It can retrieve containers with generic path path", {
  containers <- getContainers(c("Organism", "*", "Intracellu*"), sim)
  expect_equal(length(containers), 15)
})

test_that("It can retrieve all containers matching a given criteria with wild card entry", {
  containers <- getContainers(c("Organism", "Liver", "Pericentral", "*"), sim)
  expect_equal(length(containers), 5) # 4 sub compartments in liver pericentral + drug
})

test_that("It can retrieve all parameters matching a given criteria with generic path entry", {
  containers <- getContainers(c("Organism", "**", "Interstitial"), sim)
  expect_equal(length(containers), 30)
})

context("getContainer")

test_that("It can retrieve a single container by path if it exists", {
  container <- getContainer(c("Organism", "Liver", "Intracellular"), sim)
  expect_equal(container$name, "Intracellular")
})

test_that("It returns null if the  container by path does not exist", {
  container <- getContainer(c("Organism", "Liver", "TOTO", "Length"), sim)
  expect_null(container)
})
