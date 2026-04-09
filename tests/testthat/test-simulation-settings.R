sim <- loadTestSimulation("simple", loadFromCache = TRUE, addToCache = TRUE)

test_that("It can print simulation settings", {
  simulationSettings <- SimulationSettings$new(sim$get("Settings"))
  expect_snapshot(simulationSettings$print())
})
