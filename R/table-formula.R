#' @title TableFormula
#' @docType class
#' @description  A table formula of the model (Typically related to a `Quantity` such as a parameter)
#' @name Formula
#' @format NULL
TableFormula <- R6::R6Class(
  "TableFormula",
  cloneable = FALSE,
  inherit = Formula,
  active = list(
    #' @field allPoints Returns all points defined in the table formula for a `TableFormula` or `NULL`  otherwise (Read-Only).
    allPoints = function(value) {
      if (missing(value)) {
        .toObjectType(self$call("AllPointsAsArray"), ValuePoint)
      } else {
        private$throwPropertyIsReadonly("allPoints")
      }
    },
    #' @field useDerivedValues Indicates whether table values should be derived during solving. the ODE system. Default value is `TRUE`
    useDerivedValues = function(value) {
      private$.wrapProperty("UseDerivedValues", value)
    },
    #' @field xDimension The dimension in which the x values are defined (Read-Only).
    xDimension = function(value) {
      dim <- private$.wrapReadOnlyProperty("XDimension", value)
      dim$get("Name")
    }
  ),
  public = list(
    #' @description
    #' Adds one or more points to a table
    #' @param xValues x values (single value or array) in base unit for XDimension
    #' @param yValues y values (single value or array) in base unit for Dimension
    addPoints = function(xValues, yValues) {
      xValues <- c(xValues)
      yValues <- c(yValues)
      validateIsNumeric(xValues)
      validateIsNumeric(yValues)
      validateIsSameLength(xValues, yValues)
      for (i in seq_along(xValues)) {
        self$call("AddPoint", xValues[i], yValues[i])
      }
      invisible(self)
    },
    #' @description
    #' Remove the point having the same x and y from the table
    #' @param xValue xValue value in base unit for XDimension
    #' @param yValue yValue value in base unit for Dimension
    removePoint = function(xValue, yValue) {
      self$call("RemovePoint", xValue, yValue)
      invisible(self)
    },
    #' @description
    #' Remove all points from the table
    clearPoints = function() {
      self$call("ClearPoints")
      invisible(self)
    },
    #' @description
    #' Replace all points defined in the table with the new values given.
    #' This is a convenience method for calling `clearPoints` and `addPoints`
    #' @param xValues x values (single value or array) in base unit for XDimension
    #' @param yValues y values (single value or array) in base unit for Dimension
    setPoints = function(xValues, yValues) {
      self$clearPoints()
      self$addPoints(xValues, yValues)
      invisible(self)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      self$printFormula()
    },
    #' @description
    #' Returns the y defined for the x value in base unit. If not exact match is found, value will be interpolated between two existing points
    #' If the table contains no point, 0 is returned
    #' @param xValue x value for in base unit for which the yValue should be returned
    valueAt = function(xValue) {
      self$call("ValueAt", xValue)
    },
    #' @description
    #' Print the formula to the console
    printFormula = function() {
      super$printFormula()
      private$.printLine("XDimension", self$xDimension)
      private$.printLine("UseDerivedValues", self$useDerivedValues)
      for (point in self$allPoints) {
        print(point)
      }
      invisible(self)
    }
  )
)
