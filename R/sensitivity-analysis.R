#' @title SensitivityAnalysis
#' @docType class
#' @description Supports Sensitivity Analysis workflow to assess the impact of input parameters on the simulation outputs
#'
#' @export
#' @format NULL
SensitivityAnalysis <- R6::R6Class(
  "SensitivityAnalysis",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    .parameterPaths = NULL,
    .addParameterPaths = function(parameterPaths) {
      parameterPaths <- c(parameterPaths)
      if (length(parameterPaths) == 0) {
        return()
      }
      # Issue with .NET rSharp casting array with one value directly as single value instead of array
      methodName <- if (length(parameterPaths) > 1) "AddParameterPaths" else "AddParameterPath"
      self$call(methodName = methodName, parameterPaths)
      invisible(self)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param simulation Simulation for which a sensitivity analysis should be performed
    #' @param parameterPaths Vector of parameter paths to use for sensitivity calculation (optional).If undefined, the sensitivity will be performed automatically
    #' on all constant parameters of the simulation. Constant parameter means all parameters with a constant value or a formula parameter
    #' with a value that was overridden by the user
    #' @param numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in `getOSPSuiteSetting("sensitivityAnalysisConfig")`)
    #' @param variationRange Variation applied to the parameter (optional, default specified in `getOSPSuiteSetting("sensitivityAnalysisConfig")`)
    #' @return A new `SensitivityAnalysis` object.
    initialize = function(simulation,
                          parameterPaths = NULL,
                          numberOfSteps = ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps,
                          variationRange = ospsuiteEnv$sensitivityAnalysisConfig$variationRange) {
      validateIsOfType(simulation, "Simulation")
      validateIsString(parameterPaths, nullAllowed = TRUE)
      netObject <- rSharp::newObjectFromName("OSPSuite.R.Domain.SensitivityAnalysis", simulation)
      super$initialize(netObject)
      private$.simulation <- simulation
      private$.parameterPaths <- c(parameterPaths)
      self$numberOfSteps <- numberOfSteps
      self$variationRange <- variationRange
      private$.addParameterPaths(private$.parameterPaths)
    },
    #' @description
    #' Adds the parameterPaths to the list of parameter path to vary in the sensitivity analysis
    #' @param parameterPaths Parameter paths to add (single or multiple values)
    #' If no parameters were specified during creating of a `SensitivityAnalysis` (all constant parameters are considered),
    #' calling `addParameterPaths` will make only the manually added parameters being varied.
    addParameterPaths = function(parameterPaths) {
      validateIsString(parameterPaths)
      parameterPaths <- c(parameterPaths)
      private$.parameterPaths <- c(private$.parameterPaths, parameterPaths)
      private$.addParameterPaths(parameterPaths)
      invisible(self)
    },
    #' @description
    #' Removes all parameter paths defined in the Sensitivity Analysis
    clearParameterPaths = function() {
      private$.parameterPaths <- NULL
      self$call("ClearParameterPaths")
      invisible(self)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Number of steps" = self$numberOfSteps,
        "Variation range" = self$variationRange,
        "Number of parameters to vary" = if (length(private$.parameterPaths) > 0) length(private$.parameterPaths) else "Will be estimated at run time"
      ))
    }
  ),
  active = list(
    #' @field simulation Reference to the `Simulation` used to calculate or import the sensitivity analysis results (Read-Only).
    simulation = function(value) {
      private$.readOnlyProperty("simulation", value, private$.simulation)
    },
    #' @field numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in `ospsuiteEnv$sensitivityAnalysisConfig`)
    numberOfSteps = function(value) {
      private$.wrapProperty("NumberOfSteps", value, asInteger = TRUE)
    },
    #' @field variationRange Variation applied to the parameter (optional, default specified in `ospsuiteEnv$sensitivityAnalysisConfig`)
    variationRange = function(value) {
      private$.wrapProperty("VariationRange", value)
    },
    #' @field parameterPaths  List of parameters to use for sensitivity calculation.If empty, the sensitivity will be performed automatically
    #' on all constant parameters that are really in use in the simulation. Constant parameter means all parameters with a constant value or a formula parameter
    #' with a value that was overridden by the user
    parameterPaths = function(value) {
      private$.readOnlyProperty("parameterPaths", value, private$.parameterPaths)
    }
  )
)
