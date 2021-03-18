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
      DataColumn$new(private$wrapReadOnlyProperty("BaseGrid", value))
    },
    #' @field columns Returns all columns (including baseGrid defined in the data)
    columns = function(value){
      toObjectType(private$wrapReadOnlyProperty("ColumnsAsArray", value), DataColumn)
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
  )
)
