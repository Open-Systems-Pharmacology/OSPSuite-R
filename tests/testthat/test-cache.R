#  Cache
sim <- loadTestSimulation(
  "S1",
  loadFromCache = TRUE,
  addToCache = TRUE
)
cache <- ospsuiteEnv$loadedSimulationsCache

test_that("It can print simulation cache", {
  expect_snapshot(cache)
})
