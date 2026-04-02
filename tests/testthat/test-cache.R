#  Cache
sim <- loadTestSimulation(
  "simple",
  loadFromCache = FALSE,
  addToCache = TRUE
)
cache <- ospsuiteEnv$loadedSimulationsCache

test_that("It can print simulation cache", {
  expect_snapshot(cache)
})
