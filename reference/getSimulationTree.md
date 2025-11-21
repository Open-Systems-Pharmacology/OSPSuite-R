# Get simulation tree

Given a simulation file path or an instance of a simulation, traverses
the simulation structure and returns a tree like structure allowing for
intuitive navigation in the simulation tree.

## Usage

``` r
getSimulationTree(simulationOrFilePath, quantityType = "Quantity")
```

## Arguments

- simulationOrFilePath:

  Full path of the simulation to load or instance of a simulation.

- quantityType:

  A vector of strings that specify the types of the entities to be
  included in the tree. The types can be any combination of "Quantity",
  "Molecule", "Parameter" and "Observer".

## Value

A list with a branched structure representing the path tree of entities
in the simulation file that fall under the types specified in
`quantityType`. At the end of each branch is a string called 'path' that
is the path of the quantity represented by the branch.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

tree <- getSimulationTree(sim)

liver_volume_path <- tree$Organism$Liver$Volume$path
```
