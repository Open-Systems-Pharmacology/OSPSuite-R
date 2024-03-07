#' @title Molecule
#'
#' @description A molecule defined in a compartment of the system
#'
#' @details  Derived from [Quantity], please see base class documentation.
#'
#' @docType class
#' @name Molecule
#'
#' @keywords internal
#' @format NULL
Molecule <- R6::R6Class(
  "Molecule",
  cloneable = FALSE,
  inherit = Quantity,
  active = list(
    #' @field value Start value of the molecule
    value = function(value) {
      if (missing(value)) {
        ifNotNull(private$.startValue, private$.startValue$value, super$value)
      } else {
        if (is.null(private$.startValue)) {
          super$value <- value
        } else {
          private$.startValue$value <- value
        }
      }
    },
    #' @field  scaleDivisor Scale divisor. Its purpose is to reduce numerical noise and to enhance computation performance.
    #' see \url{https://docs.open-systems-pharmacology.org/working-with-mobi/mobi-documentation/model-building-components#import-molecule-and-parameter-start-values-from-excel}
    scaleDivisor = function(value) {
      private$.wrapProperty("ScaleDivisor", value)
    }
  ),
  private = list(
    .startValue = NULL
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject An `rSharp::NetObject` object.
    #' @return A new `Molecule` object.
    initialize = function(netObject) {
      super$initialize(netObject)
      # Is only set for a molecule representing a concenctration based molecule (e.g unit is umol)
      private$.startValue <- getParameter("Start value", self, stopIfNotFound = FALSE)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Path", self$path)
      initialStartValue <- private$.startValue %||% self
      initialStartValue$printQuantityValue("Initial Value")
      invisible(self)
    }
  )
)
