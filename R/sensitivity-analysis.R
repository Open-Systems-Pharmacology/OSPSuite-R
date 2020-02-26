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
      # Issue with .NET rCLR casting array with one value directly as single value instead of array
      methodName <- if (length(parameterPaths) > 1) "AddParameterPaths" else "AddParameterPath"
      rClr::clrCall(obj = self$ref, methodName = methodName, parameterPaths)
      invisible(self)
    },
    .addDynamicPKParameters = function(dynamicPKParameters) {
      dynamicPKParameters <- c(dynamicPKParameters)
      if (length(dynamicPKParameters) == 0) {
        return()
      }
      for (dynamicPKParameter in dynamicPKParameters) {
        rClr::clrCall(obj = self$ref, methodName = "AddDynamicPKParameter", dynamicPKParameter$ref)
      }
      invisible(self)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param simulation Simulation for which a sensitivity analysis should be performed
    #' @param parameterPaths List of parameters to use for sensitivity calculation (optional).If undefined, the sensitivity will be performed automatically
    #' on all constant parameters of the simulation. Constant parameter means all parameters with a constant value or a formula parameter
    #' with a value that was overriden by the user
    #' @param numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    #' @param variationRange Variation applied to the parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    #' @return A new `SensitivityAnalysis` object.
    initialize = function(
                              simulation,
                              parameterPaths = NULL,
                              numberOfSteps = ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps,
                              variationRange = ospsuiteEnv$sensitivityAnalysisConfig$variationRange) {
      validateIsOfType(simulation, Simulation)
      validateIsString(parameterPaths, nullAllowed = TRUE)
      ref <- rClr::clrNew("OSPSuite.R.Domain.SensitivityAnalysis", simulation$ref)
      super$initialize(ref)
      private$.simulation <- simulation
      private$.parameterPaths <- c(parameterPaths)
      self$numberOfSteps <- numberOfSteps
      self$variationRange <- variationRange
      private$.addParameterPaths(private$.parameterPaths)
    },
    #' @description
    #' Adds the parameterPaths to the list of parameter path to vary in the sensitivity analysis
    #' @param parameterPaths Parameter paths to add (single or multiple values)
    addParameterPaths = function(parameterPaths) {
      validateIsString(parameterPaths)
      parameterPaths <- c(parameterPaths)
      private$.parameterPaths <- c(private$.parameterPaths, parameterPaths)
      private$.addParameterPaths(parameterPaths)
      invisible(self)
    },
    #' @description
    #' Adds the dynamicPKParameters to the list of dynamic pk parameters that will be calculated for the sensitivity analysis
    #' @param dynamicPKParameters Dynamic PK Parameters to add (single or multiple values)
    addDynamicPKParameters = function(dynamicPKParameters) {
      validateIsOfType(dynamicPKParameters, DynamicPKParameter)
      private$.addDynamicPKParameters(dynamicPKParameters)
      invisible(self)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Number of steps", self$numberOfSteps)
      private$printLine("Variation range", self$variationRange)
      parameterLength <- length(private$.parameterPaths)
      private$printLine("Number of parameters to vary", if (parameterLength > 0) parameterLength else "Will be estimated at run time")
      invisible(self)
    }
  ),
  active = list(
    #' @field simulation Reference to the \code{Simulation} used to calculate or import the sensitiviy analysis results (Read-Only).
    simulation = function(value) {
      private$readOnlyProperty("simulation", value, private$.simulation)
    },
    #' @field numberOfSteps Number of steps used for the variation of each parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    numberOfSteps = function(value) {
      private$wrapIntegerProperty("NumberOfSteps", value)
    },
    #' @field variationRange Variation applied to the parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})
    variationRange = function(value) {
      private$wrapProperty("VariationRange", value)
    },
    #' @field parameterPaths  List of parameters to use for sensitivity calculation.If empty, the sensitivity will be performed automatically
    #' on all constant parameters that are really in use in the simulation. Constant parameter means all parameters with a constant value or a formula parameter
    #' with a value that was overriden by the user
    parameterPaths = function(value) {
      private$readOnlyProperty("parameterPaths", value, private$.parameterPaths)
    },
    #' @field allDynamicPKParameters Returns all dynamic PK Parameters defined for the sensitivity analysis
    allDynamicPKParameters = function(value) {
      dynamicPKParameters <- private$wrapReadOnlyProperty("AllDynamicParameters", value)
      c(toObjectType(dynamicPKParameters, DynamicPKParameter))
    }
  )
)
