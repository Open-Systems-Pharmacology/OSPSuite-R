#' @title ValuePoint
#'
#' @description A point in a table formula
#'
#' @docType class
#' @name ValuePoint
#'
#' @field x The x value of the point.
#' @field y the y value of the point.
#' @field restartSolver Indicates whether the solver should be restarted when this point is reached. Default is \code{FALSE}
#'
#' @format NULL
ValuePoint <- R6::R6Class(
  "ValuePoint",
  inherit = DotNetWrapper,
  public = list(
    print = function(...) {
      cat("  ", "x= ", self$x, ", y= ", self$y, ", restartSolver= ", self$restartSolver, "\n", sep = "")
      invisible(self)
    }
  ),
  active = list(
    x = function(value) {
      private$wrapProperty("X", value)
    },
    y = function(value) {
      private$wrapProperty("Y", value)
    },
    restartSolver = function(value) {
      private$wrapProperty("RestartSolver", value)
    }
  )
)
