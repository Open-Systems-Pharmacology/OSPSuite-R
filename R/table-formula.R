#' @title TableFormula
#' @docType class
#' @description  A table formula of the model (Typically related to a \code{Quantity} such as a parameter)
#' @field allPoints Returns all points defined in the table formulafor a \code{TableFormula} or \code{NULL}  otherwise (Read-Only).
#' @field useDerivedValues Indicates whether table values should be derived during solving. the ODE system. Default value is \code{TRUE}
#' @field xDimension The dimension in which the x values are defined (Read-Only).
#' @name Formula
#' @section Methods:
#' \describe{
#'   \item{ addPoint(x, y)}{Adds a point to the table, with x value in base unit for XDimension and y value in base unit for Dimension}
#'   \item{ removePoint(x, y)}{ Remove the point having the same x and y from the table}
#'   \item{ clearPoints()}{ Remove all points from the table}
#'   }
#' @format NULL
TableFormula <- R6::R6Class(
  "TableFormula",
  inherit = Formula,
  active = list(
    allPoints = function(value) {
      if (missing(value)) {
        return(toValuePoints(rClr::clrCall(self$ref, "AllPointsAsArray")))
      } else {
        private$throwPropertyIsReadonly("allPoints")
      }
    },
    useDerivedValues = function(value) {
      private$wrapProperty("UseDerivedValues", value)
    },
    xDimension = function(value) {
      dim <- private$wrapReadOnlyProperty("XDimension", value)
      rClr::clrGet(dim, "Name")
    }
  ),
  public = list(
    addPoint = function(x, y) {
      rClr::clrCall(self$ref, "AddPoint", x, y)
      invisible(self)
    },
    removePoint = function(x, y) {
      rClr::clrCall(self$ref, "RemovePoint", x, y)
      invisible(self)
    },
    clearPoints = function(){
      rClr::clrCall(self$ref, "ClearPoints")
      invisible(self)
    },
    print = function(...) {
      private$printClass()
      self$printFormula()
    },
    printFormula = function() {
      super$printFormula()
      private$printLine("XDimension", self$xDimension)
      private$printLine("UseDerivedValues", self$useDerivedValues)
      for (point in self$allPoints) {
        print(point)
      }
      invisible(self)
    }
  )
)
