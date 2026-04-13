#  Cache
sim <- loadTestSimulation(
  "simple",
  loadFromCache = FALSE,
  addToCache = FALSE
)
cache <- ospsuiteEnv$loadedSimulationsCache

test_that("It can print simulation cache", {
  expect_snapshot(cache)
})
