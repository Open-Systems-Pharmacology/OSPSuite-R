context("Cache")

sim <- loadTestSimulation("S1")
cache <- ospsuiteEnv$loadedSimulationsCache

test_that("It can print simulation cache", {
  expect_error(capture.output(cache$print()), NA)
})
