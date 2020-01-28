#' @title TableFormula
#' @docType class
#' @description  A table formula of the model (Typically related to a \code{Quantity} such as a parameter)
#' @name Formula
#' @format NULL
TableFormula <- R6::R6Class(
  "TableFormula",
  inherit = Formula,
  active = list(
    #' @field allPoints Returns all points defined in the table formulafor a \code{TableFormula} or \code{NULL}  otherwise (Read-Only).
    allPoints = function(value) {
      if (missing(value)) {
        toObjectType(rClr::clrCall(self$ref, "AllPointsAsArray"), ValuePoint)
      } else {
        private$throwPropertyIsReadonly("allPoints")
      }
    },
    #' @field useDerivedValues Indicates whether table values should be derived during solving. the ODE system. Default value is \code{TRUE}
    useDerivedValues = function(value) {
      private$wrapProperty("UseDerivedValues", value)
    },
    #' @field xDimension The dimension in which the x values are defined (Read-Only).
    xDimension = function(value) {
      dim <- private$wrapReadOnlyProperty("XDimension", value)
      rClr::clrGet(dim, "Name")
    }
  ),
  public = list(
    #' @description
    #' Adds a point to the table
    #' @param x x value in base unit for XDimension
    #' @param y y value in base unit for Dimension
    addPoint = function(x, y) {
      rClr::clrCall(self$ref, "AddPoint", x, y)
      invisible(self)
    },
    #' @description
    #' Remove the point having the same x and y from the table
    #' @param x x value in base unit for XDimension
    #' @param y y value in base unit for Dimension
    removePoint = function(x, y) {
      rClr::clrCall(self$ref, "RemovePoint", x, y)
      invisible(self)
    },
    #' @description
    #' Remove all points from the table
    clearPoints = function() {
      rClr::clrCall(self$ref, "ClearPoints")
      invisible(self)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      self$printFormula()
    },
    #' @description
    #' Print the formula to the console
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
