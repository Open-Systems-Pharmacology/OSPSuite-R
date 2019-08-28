# Environment that holds various global variables and settings for the ospsuite,
# It is not exported and should not be directly manipulated by other packages.
ospsuiteEnv <- new.env(parent = emptyenv())


# Reference to container task for optimization purposes only
ospsuiteEnv$containerTask <- NULL

# Separator defined in OSPSuite.Core.
ospsuiteEnv$pathSeparator <- "|"

# Default values for the formatNumerics() helper function
ospsuiteEnv$formatNumericsDigits <- 5
ospsuiteEnv$formatNumericsSmall <- 2
