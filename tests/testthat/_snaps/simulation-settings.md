# It can print simulation settings

    Code
      simulationSettings$print()
    Output
      <SimulationSettings>
      <SolverSettings>
        * useJacobian: TRUE
        * h0: 1e-10
        * hMin: 0
        * hMax: 60
        * mxStep: 100000
        * relTol: 1e-04
        * absTol: 1e-09
        * checkForNegativeValues: TRUE
      <OutputSchema>
      
      -- Output intervals --
      
      <Interval>
        * Name: Output interval
        * Start time: 0.00e+00 [min]
        * End time: 300.00 [min]
        * Resolution: 0.07 [pts/min]
      
      -- Output Selections --
      
      <OutputSelections>
        * Organism|B: Drug
        * Organism|Liver|A: Drug
        * Organism|Liver|B: Drug
        * Organism|A: Drug

