# It can print simulation results

    Code
      simResults$print()
    Output
      <SimulationResults>
        * Number of individuals: 1
      For paths:
        * Organism|Liver|A
        * Organism|Liver|B
        * Organism|A
        * Organism|B

# It can retrieve the paths of all outputs

    Code
      simResults$allQuantityPaths
    Output
      [1] "Organism|Liver|A" "Organism|Liver|B" "Organism|A"       "Organism|B"      

