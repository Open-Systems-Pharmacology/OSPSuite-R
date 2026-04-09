# It can print the simulation

    Code
      mutableSim$print()
    Output
      <Simulation>
        * Name: Simple
        * Source file: ../data/simple.pkml

# It can retrieve the name of all stationary molecules used in the model

    Code
      sim$allStationaryMoleculeNames()
    Output
      character(0)

# It can retrieve the name of all floating molecule used in the model

    Code
      sim$allFloatingMoleculeNames()
    Output
      [1] "Aciclovir"

# It can retrieve the name of all endogenous stationary molecules used in the model

    Code
      sim$allEndogenousStationaryMoleculeNames()
    Output
      character(0)

# It can retrieve the name of all xenobiotic floating molecule used in the model

    Code
      sim$allXenobioticFloatingMoleculeNames()
    Output
      [1] "Aciclovir"

