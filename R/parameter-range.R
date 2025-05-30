#' @title ParameterRange
#' @docType class
#' @description  A parameter range typically used in the definition of `PopulationCharacteristics` covariates (Height, Weight etc...)
#'
#' @format NULL
#' @export
ParameterRange <- R6::R6Class(
  "ParameterRange",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field min Minimum value for the parameter range
    min = function(value) {
      private$.wrapProperty("Min", value)
    },
    #' @field max Maximum value for the parameter range
    max = function(value) {
      private$.wrapProperty("Max", value)
    },
    #' @field unit Unit in which the value is defined
    unit = function(value) {
      private$.wrapProperty("Unit", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject Optional `NetObject` of `ParameterRange`. If not defined, a new instance will be created
    #' @param min Optional minimum value for the range
    #' @param max Optional minimum value for the range
    #' @param unit Optional unit of the specified min and max
    #' @return A new `ParameterRange` object.
    initialize = function(netObject = NULL, min = NULL, max = NULL, unit = NULL) {
      validateIsNumeric(min, nullAllowed = TRUE)
      validateIsNumeric(max, nullAllowed = TRUE)
      validateIsString(unit, nullAllowed = TRUE)
      # Assuming that if this function is called directly, PK-Sim was either initialized already
      # or should be initialized automatically
      initPKSim()
      netObject <- netObject %||% rSharp::newObjectFromName("PKSim.Core.Snapshots.ParameterRange")
      super$initialize(netObject)
      # Because of weird issue with nullable value in rClr
      # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1369
      if (!is.null(min)) {
        self$min <- min
      }
      if (!is.null(max)) {
        self$max <- max
      }
      if (!is.null(unit)) {
        self$unit <- unit
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(
        list(
          "Min" = self$min,
          "Max" = self$max,
          "Unit" = self$unit
        ),
        print_empty = TRUE
      )
    },
    #' @description
    #' Print  the parameter in one line
    #' @param caption Caption to display before the value of the parameter
    printValue = function(caption) {
      lifecycle::deprecate_warn(
        when = "12.2.0.9006",
        what = I("ospsuite::ParameterRange$printValue()"),
        with = I("ospsuite::ParameterRange$getPrintValue()")
      )
      caption <- print(paste0(caption, ": ", minDisplay, "..", maxDisplay))
    },

    #' @description
    #' Return a string for printing the parameter in one line
    #' @return A string for printing the parameter in one line
    getPrintValue = function() {
      minDisplay <- if (is.null(self$min)) "]-Inf" else paste0("[", formatNumerics(self$min), " ", self$unit)
      maxDisplay <- if (is.null(self$max)) "+Inf[" else paste0(formatNumerics(self$max), " ", self$unit, "]")
      return(paste0(minDisplay, "..", maxDisplay))
    }
  )
)

.createParameterRange <- function(min, max, unit) {
  if (is.null(min) && is.null(max)) {
    return(NULL)
  }
  return(ParameterRange$new(min = min, max = max, unit = unit))
}
