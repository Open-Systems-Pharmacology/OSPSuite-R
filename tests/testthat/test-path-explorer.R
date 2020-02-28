context("exploreSimulation")
test_that("it can explore a simulation by path", {
  simPath <- getSimulationFilePath("simple")
  tree <- exploreSimulation(simPath)

  path <- tree$Organism$Liver$Volume$path
  expect_equal(path, "Organism|Liver|Volume")
})


test_that("it can explore a simulation by instance", {
  sim <- loadTestSimulation("simple")
  tree <- exploreSimulation(sim)

  path <- tree$Organism$Liver$Volume$path
  expect_equal(path, "Organism|Liver|Volume")
})
