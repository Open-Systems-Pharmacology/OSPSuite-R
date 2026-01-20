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

# createSimulation shows warnings when showWarnings is TRUE

    Code
      newSimulation <- createSimulation(simulationConfiguration = simConfig,
        simulationName = "MySim", showWarnings = TRUE)
    Condition
      Warning in `createSimulation()`:
      Following warnings were generated during simulation creation:
       Multiple Warnings were found for 'Whole Blood'
      Multiple Warnings were found for 'Interstitial Unbound'
      Multiple Warnings were found for 'Intracellular Unbound'
      Multiple Warnings were found for 'Tissue'
      Multiple Warnings were found for 'Fraction of oral drug mass absorbed into mucosa segment'
      Multiple Warnings were found for 'Liver_pls_Liver_int'
      Multiple Warnings were found for 'Liver_pls_Liver_bc'
      Multiple Warnings were found for 'Liver_int_Liver_cell'

# createSimulation throws an error when simulation cannot be created

    Code
      expect_error(newSimulation <- createSimulation(simulationConfiguration = simConfig,
        simulationName = "MySim"))

