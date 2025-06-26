#' @title UserDefinedPKParameter
#' @docType class
#' @description  Definition of a user defined PKParameter that can be calculated on top of the standard PK Parameters
UserDefinedPKParameter <- R6::R6Class("UserDefinedPKParameter",
  inherit = PKParameter,
  cloneable = FALSE,
  active = list(
    #' @field startTime Start time in minutes for the calculation of the PK-Parameter.
    #' If not specified, the time will start at the first time point of the simulation (optional)
    startTime = function(value) {
      private$.wrapProperty("StartTime", value)
    },
    #' @field startTimeOffset Offset in minutes to apply to the start time or
    #' to the start time of the application identified by  `startApplicationIndex`. (0 by default).
    startTimeOffset = function(value) {
      private$.wrapProperty("StartTimeOffset", value)
    },
    #' @field endTime End time in minutes for the calculation of the PK-Parameter.
    #' If not specified, the time will end at the last time point of the simulation (optional)
    endTime = function(value) {
      private$.wrapProperty("EndTime", value)
    },
    #' @field endTimeOffset Offset in minutes to apply to the end time or
    #' to the start time of the application identified by `endApplicationIndex`. (0 by default).
    endTimeOffset = function(value) {
      private$.wrapProperty("EndTimeOffset", value)
    },
    #' @field startApplicationIndex 1-based Index of the application to use to determine the start time for the calculation of the PK-Parameter.
    #' If not specified, the time will start at the first time point of the simulation (optional)
    startApplicationIndex = function(value) {
      private$.wrapIndexProperty("StartApplicationIndex", value)
    },
    #' @field endApplicationIndex 1-based Index of the application to use to determine the end time for the calculation of the PK-Parameter.
    #' If not specified, the time will end at the last time point of the simulation (optional)
    endApplicationIndex = function(value) {
      private$.wrapIndexProperty("EndApplicationIndex", value)
    },
    #' @field normalizationFactor Factor to use to normalized the calculated PK-Parameter. (typically DrugMass, Dose, DosePerBodyWeight).
    #' It is the responsibility of the caller to ensure that the value is in the correct unit. (optional)
    normalizationFactor = function(value) {
      private$.wrapProperty("NormalizationFactor", value)
    },
    #' @field concentrationThreshold Used in conjunction with the `threshold` parameter type.
    #' If defined, the time at which this concentration was reached will be calculated
    concentrationThreshold = function(value) {
      private$.wrapProperty("ConcentrationThreshold", value)
    },
    #' @field standardPKParameter Based parameter to use to perform the PK-Analysis calculation.
    #' See `StandardPKParameter` enum for all possible pk parameters
    standardPKParameter = function(value) {
      private$.wrapProperty("StandardPKParameter", value, asInteger = TRUE)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "DisplayName" = self$displayName,
        "Dimension" = self$dimension,
        "DisplayUnit" = self$displayUnit,
        "StandardPKParameter" = getEnumKey(StandardPKParameter, self$standardPKParameter),
        "StartTime" = self$startTime,
        "StartTimeOffset" = self$startTimeOffset,
        "EndTime" = self$endTime,
        "EndTimeOffset" = self$endTimeOffset,
        "StartApplicationIndex" = self$startApplicationIndex,
        "EndApplicationIndex" = self$endApplicationIndex,
        "NormalizationFactor" = self$normalizationFactor,
        "ConcentrationThreshold" = self$concentrationThreshold
      ))
    }
  )
)
