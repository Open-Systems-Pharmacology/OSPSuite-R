#' @title DataSet
#' @docType class
#' @description  A wrapper around DataRepository exposing convenience methods to use and manipulate dataSets
#' (typically observed data) containing an X column, a Y column and potentially an Error columns
#' @format NULL
DataSet <- R6::R6Class(
  "DataSet",
  inherit = Printable,
  cloneable = FALSE,
  active = list(
    #' @field name The name of the DataSet
    name = function(value) {
      if (missing(value)) {
        return(self$dataRepository$name)
      }
      self$dataRepository$name <- value
    },
    #' @field xUnit Unit in which the xValues are defined
    xUnit = function(value){
      if (missing(value)) {
        return(private$.xValues$displayUnit)
      }
    },
    #' @field yUnit Unit in which the yValues are defined
    yUnit = function(value){
      if (missing(value)) {
        return(private$.yValues$displayUnit)
      }
    },
    #' @field xValues Values stored in the xUnit dimension (not necessarily in the base unit of the dimension)
    xValues = function(value){
      if (missing(value)) {
        return ( self$dataRepository$baseGrid$values)
      }
    }
  ),
  public = list(
    dataRepository = NULL,
    #' @description
    #' Initialize a new instance of the class
    #' @param dataRepository Instance of the \code{DataRepository} object to wrap
    #' @return A new `DotNetWrapper` object.
    initialize = function(dataRepository = NULL) {
      self$dataRepository <- dataRepository %||% private$createDataRepository()
      private$initializeCache()
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  ),
  private = list(
    .xValues = NULL,
    .yValues = NULL,
    createDataRepository = function() {
      # Create an empty data repository with a base grid and column
      dataRepository <- DataRepository$new()
      # Passing time for dimension for now
      xValues <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.BaseGrid", "xValues", getDimensionByName("Time")))

      # Passing concentration (mass) for dimension for now
      yValues <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.DataColumn", "yValues", getDimensionByName("Concentration (mass)"), xValues$ref))

      dataRepository$addColumn(xValues)
      dataRepository$addColumn(yValues)
      return(dataRepository)
    },

    initializeCache = function(){
      private$.xValues <- self$dataRepository$baseGrid
      #TODO we need to be a bit more careful here
      private$.yValues <- self$dataRepository$allButBaseGrid[1]
    }


  )
)
