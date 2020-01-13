context("SimulationSettings")

sim <- loadTestSimulation("S1")
settings <- sim$settings

test_that("It can print simulation settings", {
  expect_error(capture.output(settings$print()), NA)
})
