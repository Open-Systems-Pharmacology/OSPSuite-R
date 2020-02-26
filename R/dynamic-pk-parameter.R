
#' Standard PK-Parameters defined in OSPSuite
#'
#' @include enum.R
#' @export
StandardPKParameter <- enum(c(
  Unknown = 0,
  Cmax = 1,
  Cmin = 2,
  Tmax = 3,
  Tmin = 4,
  CTrough = 5,
  AucTend = 6,
  AucmTend = 7,
  AucInf = 8,
  AucTendInf = 9,
  Mrt = 10,
  FractionAucEndToInf = 11,
  Thalf = 12,
  Vss = 13,
  Vd = 14,
  Tthreshold = 15
))

#' @title DynamicPKParameter
#' @docType class
#' @description  Definition of a user defined PKParameter (dynamic) that can be calculated on top of the standard PK Parameters
DynamicPKParameter <- R6::R6Class("DynamicPKParameter",
  inherit = DotNetWrapper,
  active = list(
    #' @field name Name of the PK-Parameter
    name = function(value) {
      private$wrapProperty("Name", value)
    },
    #' @field startTime Start time for the calculation of the PK-Parameter.
    #' If not specified, the time will start at the first time point of the simulation (optional)
    startTime = function(value) {
      private$wrapProperty("StartTime", value)
    },
    #' @field startTimeOffset Offset in [min] to apply to the start time (0 by default).
    startTimeOffset = function(value) {
      private$wrapProperty("StartTimeOffset", value)
    },
    #' @field endTime End time for the calculation of the PK-Parameter.
    #' If not specified, the time will end at the last time point of the simulation (optional)
    endTime = function(value) {
      private$wrapProperty("EndTime", value)
    },
    #' @field endTimeOffset Offset in [min] to apply to the end time (0 by default).
    endTimeOffset = function(value) {
      private$wrapProperty("EndTimeOffset", value)
    },
    #' @field startApplicationIndex 1-based Index of the appplication to use to determine the start time for the calculation of the PK-Parameter.
    #' If not specified, the time will start at the first time point of the simulation (optional)
    startApplicationIndex = function(value) {
      private$wrapIndexProperty("StartApplicationIndex", value)
    },
    #' @field endApplicationIndex 1-based Index of the appplication to use to determine the end time for the calculation of the PK-Parameter.
    #' If not specified, the time will end at the last time point of the simulation (optional)
    endApplicationIndex = function(value) {
      private$wrapIndexProperty("EndApplicationIndex", value)
    },
    #' @field normalizationFactor Factor to use to normalized the calculated PK-Parameter. (typically DrugMass, Dose, Dose per bodyweight).
    #' It is the responsability of the caller to ensure that the value is in the correct unit. (optional)
    normalizationFactor = function(value) {
      private$wrapProperty("NormalizationFactor", value)
    },
    #' @field concentrationThreshold Used in conjonction with the \code{threshold} parameter type.
    #' If defined, the time at which this concentration was reached will be calculated
    concentrationThreshold = function(value) {
      private$wrapProperty("ConcentrationThreshold", value)
    },
    #' @field standardPKParameter Based parameter to use to perform the PK-Analysis calculation.
    #' See \code{StandardPKParameter} enum for all possible pk parameters
    standardPKParameter = function(value) {
      private$wrapIntegerProperty("StandardPKParameter", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref Reference to the .NET object. If null (default) a new .NET instance will be created
    #' @param name Name of the user defined PK-Parameter
    #' @param standardPKParameter What PK-Parameter should be used to perform calculation.
    #' See \code{StandardPKParameter} enum for all possible pk parameters
    #' @return A new `DynamicPKParameter` object.
    initialize = function(ref = NULL, name = NULL, standardPKParameter = NULL) {
      validateIsString(name, nullAllowed = TRUE)
      validateEnumValue(StandardPKParameter, standardPKParameter, nullAllowed = TRUE)
      ref <- ref %||% rClr::clrNew("OSPSuite.Core.Domain.PKAnalyses.DynamicPKParameter")
      super$initialize(ref)
      # Because of weird issue with nullable value in rClr
      if (!is.null(name)) {
        self$name <- name
      }
      if (!is.null(standardPKParameter)) {
        self$standardPKParameter <- standardPKParameter
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Name", self$name)
      private$printLine("StandardPKParameter", getEnumKey(StandardPKParameter, self$standardPKParameter))
      private$printLine("StartTime", self$startTime)
      private$printLine("startTimeOffset", self$startTimeOffset)
      private$printLine("EndTime", self$endTime)
      private$printLine("EndTimeOffset", self$endTimeOffset)
      private$printLine("StartApplicationIndex", self$startApplicationIndex)
      private$printLine("EndApplicationIndex", self$endApplicationIndex)
      private$printLine("NormalizationFactor", self$normalizationFactor)
      private$printLine("ConcentrationThreshold", self$concentrationThreshold)
      invisible(self)
    }
  )
)
