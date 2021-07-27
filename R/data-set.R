#' Supported types of the error
#' @include enum.R
#' @export
DataErrorType <- enum(c(
  "ArithmeticStdDev",
  "GeometricStdDev"
))

#' @title DataSet
#' @docType class
#' @description  A class for storage of numerical x- and y-value pairs and optional error for y-values.
#' @export
#' @format NULL
DataSet <- R6::R6Class(
  "DataSet",
  inherit = Printable,
  cloneable = FALSE,
  active = list(
    #' @field name The name of the DataSet
    name = function(value) {
      if (missing(value)) {
        return(private$.dataRepository$name)
      }
      private$.dataRepository$name <- value
    },
    #' @field xDimension Dimension in which the xValues are defined
    xDimension = function(value) {
      if (missing(value)) {
        return(private$.xColumn$dimension)
      }
      private$.setColumnDimension(private$.xColumn, value)
    },
    #' @field xUnit Unit in which the xValues are defined
    xUnit = function(value) {
      if (missing(value)) {
        return(private$.xColumn$displayUnit)
      }
      private$.setColumnUnit(private$.xColumn, value)
    },
    #' @field xValues Values stored in the xDimension dimension (not necessarily in the base unit)
    xValues = function(values) {
      if (missing(values)) {
        return(private$.getColumnValues(private$.xColumn))
      }

      #TODO how to handle a situation when the lenght of new x values differs from the length of y values?
      private$.setColumnValues(private$.xColumn, values)
    },
    #' @field yDimension Dimension in which the yValues are defined
    yDimension = function(value) {
      if (missing(value)) {
        return(private$.yColumn$dimension)
      }
      private$.setColumnDimension(private$.yColumn, value)
    },
    #' @field yUnit Unit in which the yValues are defined
    yUnit = function(value) {
      if (missing(value)) {
        return(private$.yColumn$displayUnit)
      }
      private$.yColumn$displayUnit <- value
    },
    #' @field yValues Values stored in the yDimension dimension (not necessarily in the base unit)
    yValues = function(values) {
      if (missing(values)) {
        return(private$.getColumnValues(private$.yColumn))
      }
      validateIsSameLength(self$xValues, values)
      private$.setColumnValues(private$.yColumn, values)
    },

    #' @field yErrorType Type of the error - geometric or arithmetic.
    #' When changing from arithmetic to geometric error, the values are considered in as fraction (1 = 100%).
    #' When changing from geometric to arithmetic, the values are set to the same unit as \code{yErrorUnit}.
    yErrorType = function(value) {
      if (missing(value)) {
        dataInfo <- rClr::clrGet(private$.yErrorColumn$ref, "DataInfo")
        errorTypeEnumVal <- rClr::clrGet(dataInfo, "AuxiliaryType")
        return(netEnumName("OSPSuite.Core.Domain.Data.AuxiliaryType", errorTypeEnumVal))
      }
      private$.setErrorType(value)
    },
    #' @field yErrorUnit Unit in which the yErrorValues are defined. For arithmetic error, the unit must be valid
    #' for \code{yDimension}. For geometric error, the unit must be valid for \code{Dimensionless}.
    yErrorUnit = function(value) {
      if (missing(value)) {
        return(private$.yErrorColumn$displayUnit)
      }
      private$.yErrorColumn$displayUnit <- value
    },
    #' @field yErrorValues Values of error stored in the yErrorUnit unit
    yErrorValues = function(values) {
      if (missing(values)) {
        return(private$.getColumnValues(private$.yErrorColumn))
      }

      validateIsSameLength(self$yValues, values)
      private$.setColumnValues(private$.yErrorColumn, values)
    },

    #' @field metaData Returns a named list of meta data defined for the data set.
    metaData = function(value) {
      if (missing(value)) {
        return(private$.dataRepository$metaData)
      }
      private$throwPropertyIsReadonly("metaData")
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param dataRepository Instance of the \code{DataRepository} object to wrap.
    #' If \code{NULL}, an empty \code{DataRepository} is created.
    #' @return A new `DataSet` object.
    initialize = function(dataRepository = NULL) {
      private$.dataRepository <- dataRepository %||% private$.createDataRepository()
      private$.initializeCache()
    },

    #' @description
    #' Adds a new entry to meta data list or changes its value if the name is already present.
    #' If \code{value} is \code{NULL}, the entry with corresponding name is deleted from meta data set.
    #'
    #' @param name Name of new meta data list entry
    #' @param value Value of new meta data list entry
    addMetaData = function(name, value) {
      if (length(name) != 1) {
        stop(messages$errorMultipleMetaDataEntries())
      }
      validateIsString(name)

      # TODO
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("X dimension", self$xDimension)
      private$printLine("X unit", self$xUnit)
      private$printLine("Y dimension", self$yDimension)
      private$printLine("Y unit", self$yUnit)
      private$printLine("Error type", self$yErrorType)
      private$printLine("Error unit", self$yErrorUnit)
      private$printLine("Meta data")
      print(self$metaData)
      invisible(self)
    }
  ),
  private = list(
    .dataRepository = NULL,
    .xColumn = NULL,
    .yColumn = NULL,
    .yErrorColumn = NULL,
    .setColumnValues = function(column, values) {
      # values are set in the display unit. We need to make sure we convert them to the base unit
      valuesInBaseUnit <- toBaseUnit(quantityOrDimension = column$dimension, values = values, unit = column$displayUnit)
      column$values <- valuesInBaseUnit
      invisible(self)
    },
    .getColumnValues = function(column) {
      # we need to convert the values in the display unit
      toUnit(quantityOrDimension = column$dimension, values = column$values, targetUnit = column$displayUnit)
    },
    .setColumnDimension = function(column, value){
      # no need to update anything if we are setting the same values
      if(column$dimension == value){
        return()
      }

      # save the values in their display unit before updating
      values <- private$.getColumnValues(column)

      #now we need to update dimension (display unit will be the default one as per .NET implementation)
      column$dimension <- value

      private$.setColumnValues(column, values)
    },
    .setColumnUnit = function(column, unit){
      # no need to update anything if we are setting the same values
      if(column$displayUnit == unit){
        return()
      }

      # save the values in their display unit before updating
      values <- private$.getColumnValues(column)

      #now we need to update dimension and display unit
      column$displayUnit <- unit

      private$.setColumnValues(column, values)
    },
    .setErrorType = function(errorType){
      #If error type does not change, do nothing
      if (errorType == self$yErrorType){
        invisible(self)
      }

      validateEnumValue(errorType, DataErrorType)
      column <- private$.yErrorColumn
      values <- private$.getColumnValues(column)

      dataInfo <- rClr::clrGet(column$ref, "DataInfo")
      rClr::clrSet(dataInfo, "AuxiliaryType", rClr::clrGet("OSPSuite.Core.Domain.Data.AuxiliaryType", errorType))

      # Geometric to arithmetic - set to the same dimension and unit as yValues
      if (errorType == DataErrorType$ArithmeticStdDev){
        private$.setColumnDimension(column, self$yDimension)
        private$.setColumnUnit(column, self$yUnit)
      }

      # Arithmetic to geometric - set to dimensionless
      if (errorType == DataErrorType$ArithmeticStdDev){
        private$.setColumnDimension(column, ospDimensions$Dimensionless)
      }

      private$.setColumnValues(column, values)
    },

    .createDataRepository = function() {
      # Create an empty data repository with a base grid and columns
      dataRepository <- DataRepository$new()
      # Passing time for dimension for now
      xValues <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.BaseGrid", "xValues", getDimensionByName(ospDimensions$Time)))

      # Passing concentration (mass) for dimension for now
      yValues <- DataColumn$new(rClr::clrNew("OSPSuite.Core.Domain.Data.DataColumn", "yValues", getDimensionByName(ospDimensions$`Concentration (mass)`), xValues$ref))

      dataRepository$addColumn(xValues)
      dataRepository$addColumn(yValues)
      return(dataRepository)
    },

    .initializeCache = function() {
      private$.xColumn <- private$.dataRepository$baseGrid
      # TODO we need to be a bit more careful here
      private$.yColumn <- private$.dataRepository$allButBaseGrid[[1]]

      dataRepositoryTask <- getNetTask("DataRepositoryTask")
      netYErrorColumn <- rClr::clrCall(dataRepositoryTask, "GetErrorColumn", private$.yColumn$ref)
      #If the repository does not have an error column, create a new one
      if (is.null(netYErrorColumn)){
        netYErrorColumn <- rClr::clrCall(dataRepositoryTask, "AddErrorColumn", private$.yColumn$ref, "yErrorValues", DataErrorType$ArithmeticStdDev)
      }
      yErrorColumn <- DataColumn$new(netYErrorColumn)
      private$.dataRepository$addColumn(yErrorColumn)
      private$.yErrorColumn <- yErrorColumn
    }
  )
)
