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
        * Name: Simulation interval high resolution
        * Start time: 0.00e+00 [min]
        * End time: 120.00 [min]
        * Resolution: 0.33 [pts/min]
      <Interval>
        * Name: Simulation interval low resolution
        * Start time: 120.00 [min]
        * End time: 1440.00 [min]
        * Resolution: 0.07 [pts/min]
      
      -- Output Selections --
      
      <OutputSelections>
        * Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood):
        Drug, Observer
        * Organism|ArterialBlood|Plasma|Caffeine|Concentration in container: Drug,
        Observer
        * Organism|ArterialBlood|Plasma|Caffeine|Plasma Unbound: Drug, Observer
        * Organism|ArterialBlood|BloodCells|Caffeine|Concentration in container:
        Drug, Observer
        * Organism|ArterialBlood|Caffeine|Whole Blood: Drug, Observer

