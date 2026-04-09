# `runSimulation()` is deprecated

    Code
      sim <- loadTestSimulation("simple", loadFromCache = TRUE)
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
      The following warnings were generated during simulation creation:
        Container 'MySim|Organism|EndogenousIgG' was not found. Parameter 'Kd (FcRn, endogenous IgG) in endosomal space' will be ignored.
       Container 'MySim|Organism|EndogenousIgG|Plasma' was not found. Parameter 'Start concentration of free endogenous IgG (plasma)' will be ignored.
       Container 'MySim|Organism|EndogenousIgG|Endosome' was not found. Parameter 'Start concentration of free FcRn (endosome)' will be ignored.
      Multiple Warnings were found for 'Liver_int_Liver_cell'
      Multiple Warnings were found for 'Liver_pls_Liver_bc'
      Multiple Warnings were found for 'Liver_pls_Liver_int'

# createSimulation throws an error when simulation cannot be created

    Code
      expect_error(newSimulation <- createSimulation(simulationConfiguration = simConfig,
        simulationName = "MySim"))

