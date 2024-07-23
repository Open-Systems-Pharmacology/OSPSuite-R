#' @keywords internal
.getSuiteVersion <- function() {
  version <- getNamespaceVersion("ospsuite")
  first <- utils::head(unlist(gregexpr(pattern = "\\.", version)), 1)
  return(unname(substr(version, 1, first - 1)))
}

# Environment that holds various global variables and settings for the ospsuite,
# It is not exported and should not be directly manipulated by other packages.
ospsuiteEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
ospsuiteEnv$packageName <- "ospsuite"

# Major version of the suite. Corresponds to the version of installed `ospsuite-r`
ospsuiteEnv$suiteVersion <- .getSuiteVersion()

# Separator defined in OSPSuite.Core.
ospsuiteEnv$pathSeparator <- "|"

# Number of cores to use for simulations and sensitivity. Default to number of cores on the machine - 1
ospsuiteEnv$numberOfCores <- function() {
  parallel::detectCores() - 1
}

# Specifies the default behavior fo progress visualization. By default FALSE
ospsuiteEnv$showProgress <- FALSE

# Specifies the symbol used for µ. This will be set by the .NET layer
ospsuiteEnv$muSymbol <- "\U00B5"

# Cache of the so far loaded simulations. The keys are the paths to the pkml file.
ospsuiteEnv$loadedSimulationsCache <- Cache$new("Simulation")

# Default value for sensitivity Analysis calculations
ospsuiteEnv$sensitivityAnalysis <- new.env(parent = emptyenv())

# Default number of steps a new SA instance
ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps <- 2

# Default variation range for a new SA instance
ospsuiteEnv$sensitivityAnalysisConfig$variationRange <- 0.1

# Represents the threshold of sensitivity that we want to achieve.
# A value of 0.9 will select all parameters contributing to 90% total sensitivity
ospsuiteEnv$sensitivityAnalysisConfig$totalSensitivityThreshold <- 0.9

# Small value added to zero when calculating log
ospsuiteEnv$LOG_SAFE_EPSILON <- 1e-20

#' Names of the settings stored in ospsuiteEnv. Can be used with `getOSPSuiteSetting()`
#' @include utilities.R
#' @export
ospsuiteSettingNames <- enum(names(ospsuiteEnv))

#' Get the value of a global ospsuite-R setting.
#'
#' @param settingName String name of the setting
#'
#' @return Value of the setting stored in ospsuiteEnv. If the setting does not exist, an error is thrown.
#' @export
#'
#' @examples
#' getOSPSuiteSetting("suiteVersion")
#' getOSPSuiteSetting("sensitivityAnalysisConfig")$totalSensitivityThreshold
getOSPSuiteSetting <- function(settingName) {
  if (!(any(names(ospsuiteEnv) == settingName))) {
    stop(messages$errorPackageSettingNotFound(settingName, ospsuiteEnv))
  }

  obj <- ospsuiteEnv[[settingName]]
  # Evalulate if the object is a function. This is required since some properties are defined as function reference
  if (is.function(obj)) {
    return(obj())
  }

  return(obj)
}
