#' @title ValuePoint
#'
#' @description An entry (x, y) in a table formula
#'
#' @docType class
#' @name ValuePoint
#'
#' @format NULL
ValuePoint <- R6::R6Class(
  "ValuePoint",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      cat("  ", "x= ", self$x, ", y= ", self$y, ", restartSolver= ", self$restartSolver, "\n", sep = "")
      invisible(self)
    }
  ),
  active = list(
    #' @field x The x value of the point.
    x = function(value) {
      private$wrapProperty("X", value)
    },
    #' @field y the y value of the point.
    y = function(value) {
      private$wrapProperty("Y", value)
    },
    #' @field restartSolver Indicates whether the solver should be restarted when this point is reached. Default is \code{FALSE}
    restartSolver = function(value) {
      private$wrapProperty("RestartSolver", value)
    }
  )
)
