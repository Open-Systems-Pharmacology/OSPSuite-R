#  Cache
sim <- get_sim_s1_immutable()
cache <- ospsuiteEnv$loadedSimulationsCache

test_that("It can print simulation cache", {
  expect_snapshot(cache)
})
