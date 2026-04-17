# SimulationConfiguration can be created from a simulation loaded from PKML

    Code
      configurationFromPKML
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Vergin 1995 IV --
      
        * Selected Initial Conditions: Initial Conditions
        * Selected Parameter Values: Parameter Values
      
      --------------------------------------------------------------------------------
      Individual:
        * Vergin_1995_IV
      Expression profiles:
        * CYP3A4|Human|Healthy
      Partition coefficient method overrides:
        * Aciclovir: PK-Sim Standard
      Cellular permeability method overrides:
        * Aciclovir: PK-Sim Standard

# SimulationConfiguration can be created from a simulation loaded from a MoBi project with selected IC and PV BBs

    Code
      configurationFromProject
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- ExtModule_noIC_noPV --
      
        * Selected Initial Conditions: NULL
        * Selected Parameter Values: NULL
      
      -- ExtModule_3IC_3PV --
      
        * Selected Initial Conditions: IC1
        * Selected Parameter Values: PV1
      
      --------------------------------------------------------------------------------
      Individual:
        * DefaultIndividual
      Expression profiles:
        * CYP3A4|Human|Healthy
        * UGT2B6|Human|Healthy

# SimulationConfiguration can be created from a simulation loaded from a MoBi project with selected PV but no IC BBs

    Code
      configurationFromProject
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- ExtModule_noIC_noPV --
      
        * Selected Initial Conditions: NULL
        * Selected Parameter Values: NULL
      
      -- ExtModule_3IC_3PV --
      
        * Selected Initial Conditions: NULL
        * Selected Parameter Values: PV2
      
      --------------------------------------------------------------------------------
      Individual:
        * DefaultIndividual
      Expression profiles:
        * UGT2B6|Human|Healthy
        * CYP3A4|Human|Healthy

# setPartitionCoefficientMethods stores a valid method for a molecule

    Code
      config
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Simple --
      
        * Selected Initial Conditions: S
        * Selected Parameter Values: S
      
      --------------------------------------------------------------------------------
      Individual:
      Expression profiles:
      Partition coefficient method overrides:
        * A: Berezhkovskiy

# setPartitionCoefficientMethods overwrites silently on repeated call for same molecule

    Code
      config
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Simple --
      
        * Selected Initial Conditions: S
        * Selected Parameter Values: S
      
      --------------------------------------------------------------------------------
      Individual:
      Expression profiles:
      Partition coefficient method overrides:
        * A: Rodgers and Rowland

# setPartitionCoefficientMethods with NULL removes the override

    Code
      config
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Simple --
      
        * Selected Initial Conditions: S
        * Selected Parameter Values: S
      
      --------------------------------------------------------------------------------
      Individual:
      Expression profiles:

# setPartitionCoefficientMethods stores overrides for multiple molecules

    Code
      config
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Simple --
      
        * Selected Initial Conditions: S
        * Selected Parameter Values: S
      
      --------------------------------------------------------------------------------
      Individual:
      Expression profiles:
      Partition coefficient method overrides:
        * A: Berezhkovskiy
        * B: Schmitt

# setCellularPermeabilityMethods stores a valid method for a molecule

    Code
      config
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Simple --
      
        * Selected Initial Conditions: S
        * Selected Parameter Values: S
      
      --------------------------------------------------------------------------------
      Individual:
      Expression profiles:
      Cellular permeability method overrides:
        * A: Charge dependent Schmitt

# setCellularPermeabilityMethods with NULL removes the override

    Code
      config
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Simple --
      
        * Selected Initial Conditions: S
        * Selected Parameter Values: S
      
      --------------------------------------------------------------------------------
      Individual:
      Expression profiles:

