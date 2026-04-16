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
        Container 'MySim|Organism|Bone|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Brain|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Fat|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Gonads|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Heart|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Kidney|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Stomach|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|SmallIntestine|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|SmallIntestine|Mucosa|Duodenum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|SmallIntestine|Mucosa|UpperJejunum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|SmallIntestine|Mucosa|LowerJejunum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|SmallIntestine|Mucosa|UpperIleum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|SmallIntestine|Mucosa|LowerIleum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Mucosa|Caecum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Mucosa|ColonAscendens|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Mucosa|ColonTransversum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Mucosa|ColonDescendens|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Mucosa|ColonSigmoid|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|LargeIntestine|Mucosa|Rectum|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Liver|Periportal|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Liver|Pericentral|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Lung|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Muscle|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Pancreas|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Skin|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|Spleen|Endosome|CYP3A4' was not found. Parameter 'Initial concentration' will be ignored.
       Container 'MySim|Organism|EndogenousIgG' was not found. Parameter 'Kd (FcRn, endogenous IgG) in endosomal space' will be ignored.
       Container 'MySim|Organism|EndogenousIgG|Plasma' was not found. Parameter 'Start concentration of free endogenous IgG (plasma)' will be ignored.
       Container 'MySim|Organism|EndogenousIgG|Endosome' was not found. Parameter 'Start concentration of free FcRn (endosome)' will be ignored.
      Multiple Warnings were found for 'Liver_int_Liver_cell'
      Multiple Warnings were found for 'Liver_pls_Liver_bc'
      Multiple Warnings were found for 'Liver_pls_Liver_int'
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Bone|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Brain|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Fat|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Gonads|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Heart|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Kidney|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Stomach|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|SmallIntestine|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|SmallIntestine|Mucosa|Duodenum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|SmallIntestine|Mucosa|UpperJejunum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|SmallIntestine|Mucosa|LowerJejunum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|SmallIntestine|Mucosa|UpperIleum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|SmallIntestine|Mucosa|LowerIleum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Mucosa|Caecum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Mucosa|ColonAscendens|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Mucosa|ColonTransversum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Mucosa|ColonDescendens|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Mucosa|ColonSigmoid|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|LargeIntestine|Mucosa|Rectum|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Liver|Periportal|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Liver|Pericentral|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Lung|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Muscle|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Pancreas|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Skin|Endosome' that cannot be resolved
      Initial condition defined for molecule 'CYP3A4' in a container 'Organism|Spleen|Endosome' that cannot be resolved

# createSimulation throws an error when simulation cannot be created

    Code
      newSimulation <- createSimulation(simulationConfiguration = simConfig,
        simulationName = "MySim")
    Condition
      Error in `createSimulation()`:
      ! Cannot create simulation. The following errors were generated during simulation creation:
       Cannot create application 'Intravenous_Transport': molecule 'Aciclovir' not available in the target container 'Plasma'

# createSimulation shows warnings when simulation creation issues warnings

    Code
      newSimulation <- createSimulation(simulationConfiguration = simConfig,
        simulationName = "MySim")

