# `runSimulation()` is deprecated

    Code
      sim <- loadTestSimulation("S1", loadFromCache = TRUE)
      results <- runSimulation(sim)
    Condition
      Warning:
      `runSimulation()` was deprecated in ospsuite 12.0.0.
      i Please use `runSimulations()` instead.
    Code
      expect_equal(results$count, 1)

