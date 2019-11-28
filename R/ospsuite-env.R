# Environment that holds various global variables and settings for the ospsuite,
# It is not exported and should not be directly manipulated by other packages.
ospsuiteEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
ospsuiteEnv$packageName <- "ospsuite"

# Reference to container task for optimization purposes only
ospsuiteEnv$containerTask <- NULL

# Separator defined in OSPSuite.Core.
ospsuiteEnv$pathSeparator <- "|"

# Default values for the formatNumerics() helper function
ospsuiteEnv$formatNumericsDigits <- 5
ospsuiteEnv$formatNumericsSmall <- 2

# Cache of the so far loaded simulations. The keys are the paths to the pkml file.
ospsuiteEnv$loadedSimulationsCache <- Cache$new("Simulation")

# Default value for sensitivity Analysis variation
ospsuiteEnv$sensitivityAnalysis  <- new.env(parent = emptyenv())
ospsuiteEnv$sensitivityAnalysis$defaultNumberOfSteps <- 4
ospsuiteEnv$sensitivityAnalysis$defaultVariationRange <- 0.1

