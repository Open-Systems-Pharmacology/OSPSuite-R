sim <- loadTestSimulation("S1")

test_that("It can print simulation settings", {
  simulationSettings <- SimulationSettings$new(sim$get("Settings"))
  expect_snapshot(simulationSettings$print())
})
