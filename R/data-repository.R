#' @title DataRepository
#' @docType class
#' @description  An object typically holding observed data
#' @format NULL
DataRepository <- R6::R6Class(
  "DataRepository",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field baseGrid Returns the base column for the population (typically time column)
    baseGrid = function(value) {
      if (missing(value)) {
        if (is.null(private$.baseGrid)) {
          private$.baseGrid <- DataColumn$new(private$wrapReadOnlyProperty("BaseGrid", value))
        }
        return(private$.baseGrid)
      }

      private$throwPropertyIsReadonly("baseGrid")
    },
    #' @field columns Returns all columns (including baseGrid defined in the data)
    columns = function(value) {
      if (missing(value)) {
        if (is.null(private$.columns)) {
          private$.columns <- c(self$baseGrid, self$allButBaseGrid)
        }
        return(private$.columns)
      }
      private$throwPropertyIsReadonly("columns")
    },
    #' @field columns Returns all columns (including baseGrid defined in the data)
    allButBaseGrid = function(value) {
      if (missing(value)) {
        if (is.null(private$.allButBaseGrid)) {
          private$.allButBaseGrid <- toObjectType(private$wrapReadOnlyProperty("AllButBaseGridAsArray", value), DataColumn)
        }
        return(private$.allButBaseGrid)
      }
      private$throwPropertyIsReadonly("columns")
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  ),
  private = list(
    .columns = NULL,
    .allButBaseGrid = NULL,
    .baseGrid = NULL
  )
)
