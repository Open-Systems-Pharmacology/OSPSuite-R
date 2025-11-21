# SimulationConfiguration can be created from a simulation loaded from PKML

    Code
      configurationFromPKML
    Output
      <SimulationConfiguration>
      
      -- Modules ---------------------------------------------------------------------
      
      -- Vergin 1995 IV --
      
        * Selected Initial Conditions: Vergin 1995 IV
        * Selected Parameter Values: Vergin 1995 IV
      Individual:
      Expression profiles:

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
      Individual:
        * DefaultIndividual
      Expression profiles:
        * UGT2B6|Human|Healthy
        * CYP3A4|Human|Healthy

