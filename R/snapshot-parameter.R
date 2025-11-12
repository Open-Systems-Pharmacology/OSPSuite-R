#' @title SnapshotParameter
#' @docType class
#' @description  A parameter typically used in the definition of `IndividualCharacteristics` covariates (Height, Weight etc...)
#'
#' @format NULL
#' @export
SnapshotParameter <- R6::R6Class(
  "SnapshotParameter",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field value Parameter value
    value = function(value) {
      private$.wrapProperty("Value", value)
    },
    #' @field unit Unit in which the value is defined
    unit = function(value) {
      private$.wrapProperty("Unit", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject Optional `NetObject`. If not defined, a new instance will be created
    #' @param value Optional value of the parameter.
    #' @param unit Optional unit of the value specified.
    #' @return A new `SnapshotParameter` object.
    initialize = function(netObject = NULL, value = NULL, unit = NULL) {
      validateIsNumeric(value, nullAllowed = TRUE)
      validateIsString(unit, nullAllowed = TRUE)
      # Assuming that if this function is called directly, PK-Sim was either initialized already
      # or should be initialized automatically
      initPKSim()
      netObject <- netObject %||%
        rSharp::newObjectFromName("OSPSuite.Core.Snapshots.Parameter")
      super$initialize(netObject)
      # Because of weird issue with nullable value in rClr
      # https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1369
      if (!is.null(value)) {
        self$value <- value
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
          "Value" = self$value,
          "Unit" = self$unit
        ),
        print_empty = TRUE
      )
    },
    #' @description
    #' Print the parameter in one line
    #' @param caption Caption to display before the value of the parameter
    printValue = function(caption) {
      lifecycle::deprecate_warn(
        when = "12.2.0.9006",
        what = I("ospsuite::SnapshotParameter$printValue()"),
        with = I("ospsuite::SnapshotParameter$getPrintValue()")
      )
      ospsuite.utils::ospPrintItems(list(caption = self$getPrintValue()))
    },

    #' @description
    #' Return a string for printing the parameter in one line
    #' @return A string for printing the parameter in one line
    getPrintValue = function() {
      paste0(formatNumerics(self$value), " [", self$unit, "]")
    }
  )
)

.createSnapshotParameter <- function(value, unit) {
  if (is.null(value) || is.na(value)) {
    return(NULL)
  }
  return(SnapshotParameter$new(value = value, unit = unit))
}
