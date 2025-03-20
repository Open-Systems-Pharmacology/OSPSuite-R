#' @title Parameter
#'
#' @description A model parameter
#'
#' @details  Derived from [Quantity], please see base class documentation.
#'
#' @docType class
#' @name Parameter
#'
#' @keywords Parameter
#' @format NULL
Parameter <- R6::R6Class(
  "Parameter",
  cloneable = FALSE,
  inherit = Quantity,
  private = list(
    .rhsFormula = NULL
  ),
  active = list(
    #' @field isStateVariable Returns `TRUE` is the parameter has a RHS otherwise `FALSE`.
    #' Setting the value to `FALSE` will delete the RHS Formula.
    #' Setting it to `TRUE` is not currently supported and will throw an error.
    isStateVariable = function(value) {
      hasRHSFormula <- !is.null(private$.rhsFormula)
      if (missing(value)) {
        return(hasRHSFormula)
      }
      validateIsLogical(value)

      # Set to TRUE AND we have a rhs, nothing to do
      if (value && hasRHSFormula) {
        return()
      }

      # Set to true and no RHS => error
      if (value) {
        stop(messages$errorCannotSetRHSFormula)
      }

      # we are deleting the RHS Formula
      private$.rhsFormula <- NULL
      self$call("ClearRHSFormula")
    },
    #' @field rhsFormula An instance of a `Formula` object representing the RHS Formula (Read-Only)
    rhsFormula = function(value) {
      private$.readOnlyProperty("rhsFormula", value, private$.rhsFormula)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject An `rSharp::NetObject` object.
    #' @return A new `Parameter` object.
    initialize = function(netObject) {
      super$initialize(netObject)
      rhsFormula <- self$get("RHSFormula")
      private$.rhsFormula <- ifNotNull(rhsFormula, Formula$new(rhsFormula))
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      super$print()
      if (self$isStateVariable) {
        ospsuite.utils::ospPrintHeader("State variable", level = 2)
        ospsuite.utils::ospPrintItems(list(
          "isStateVariable" = self$isStateVariable
        ))
        ospsuite.utils::ospPrintHeader("RHSFormula", level = 3)
        self$rhsFormula$printFormula()
      }
    }
  )
)
