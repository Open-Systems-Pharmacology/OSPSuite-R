#' Supported types of the error
#'
#' @export
DataErrorType <- enum(c(
  "ArithmeticStdDev",
  "GeometricStdDev"
))

#' @title DataSet
#' @docType class
#' @description A class for storage of numerical x- and y-value pairs and
#'   optional error for y-values.
#' @export
#' @format NULL
DataSet <- R6::R6Class(
  "DataSet",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(

    #' @field name The name of the DataSet
    name = function(value) {
      if (missing(value)) {
        return(private$.dataRepository$name)
      }
      private$.dataRepository$name <- value
    },

    #' @field dataRepository The underlying DataRepository object
    dataRepository = function(value) {
      if (missing(value)) {
        return(private$.dataRepository)
      }
      private$.throwPropertyIsReadonly("dataRepository")
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

    #' @field xValues Values stored in the xUnit. This field is read-only.
    #' Use `$setValues()` to change the values.
    xValues = function(values) {
      if (missing(values)) {
        return(private$.getColumnValues(private$.xColumn))
      }
      private$.throwPropertyIsReadonly("xValues")
    },

    #' @field yDimension Dimension in which the yValues are defined
    yDimension = function(value) {
      if (missing(value)) {
        return(private$.yColumn$dimension)
      }
      private$.setColumnDimension(private$.yColumn, value)
      # Also update the dimension of yError
      if (!is.null(private$.yErrorColumn)) {
        private$.setColumnDimension(private$.yErrorColumn, value)
      }
    },

    #' @field yUnit Unit in which the yValues are defined
    yUnit = function(value) {
      if (missing(value)) {
        return(private$.yColumn$displayUnit)
      }
      private$.setColumnUnit(private$.yColumn, value)
    },

    #' @field yValues Values stored in the yUnit. This field is read-only.
    #' Use `$setValues()` to change the values.
    yValues = function(values) {
      if (missing(values)) {
        return(private$.getColumnValues(private$.yColumn))
      }
      private$.throwPropertyIsReadonly("yValues")
    },

    #' @field yErrorType Type of the error - geometric or arithmetic.
    #' When changing from arithmetic to geometric error, the values are
    #' considered in as fraction (1 = 100%).
    #' When changing from geometric to arithmetic, the values are set to the
    #' same unit as `yErrorUnit`.
    #' In case no yError is defined, the value is `NULL` and cannot be changed
    yErrorType = function(value) {
      if (missing(value)) {
        if (is.null(private$.yErrorColumn)) {
          return(NULL)
        }

        dataInfo <- private$.yErrorColumn$get("DataInfo")
        errorTypeEnumVal <- dataInfo$get("AuxiliaryType")
        return(.netEnumName("OSPSuite.Core.Domain.Data.AuxiliaryType", errorTypeEnumVal))
      }
      private$.setErrorType(value)
    },

    #' @field yErrorUnit Unit in which the yErrorValues are defined. For
    #'   arithmetic error, the unit must be valid for `yDimension`. For
    #'   geometric error, the unit must be valid for `Dimensionless`.
    #'   In case no yError is defined, the value is `NULL` and cannot be changed
    yErrorUnit = function(value) {
      if (missing(value)) {
        # Do not have to check for NULL here becase NULL$something is NULL
        return(private$.yErrorColumn$displayUnit)
      }

      if (!is.null((private$.yErrorColumn))) {
        private$.setColumnUnit(private$.yErrorColumn, value)
      }
      invisible(self)
    },

    #' @field yErrorValues Values of error stored in the yErrorUnit unit. This field is read-only.
    #' Use `$setValues()` to change the values.
    #' In case no yError is defined, the value is `NULL` and cannot be changed.
    #' Use `$setValues()` to change the values.
    yErrorValues = function(values) {
      if (missing(values)) {
        if (is.null(private$.yErrorColumn)) {
          return(NULL)
        }

        return(private$.getColumnValues(private$.yErrorColumn))
      }
      private$.throwPropertyIsReadonly("yErrorValues")
    },

    #' @field molWeight Molecular weight of the yValues in g/mol
    molWeight = function(value) {
      if (missing(value)) {
        molWeight <- private$.yColumn$molWeight
        if (is.null(molWeight)) {
          return(NULL)
        }
        return(toUnit(quantityOrDimension = ospDimensions$`Molecular weight`, values = molWeight, targetUnit = ospUnits$`Molecular weight`$`g/mol`))
      }

      private$.yColumn$molWeight <- toBaseUnit(
        quantityOrDimension = ospDimensions$`Molecular weight`,
        values = value,
        unit = ospUnits$`Molecular weight`$`g/mol`
      )
    },

    #' @field LLOQ Lower Limit Of Quantification.
    #' Value in yUnit associated with the yValues
    LLOQ = function(value) {
      if (missing(value)) {
        # Value in base unit
        lloq <- private$.yColumn$LLOQ
        if (is.null(lloq)) {
          return(NULL)
        }

        return(toUnit(quantityOrDimension = private$.yColumn$dimension, values = lloq, targetUnit = private$.yColumn$displayUnit))
      }

      # Only one LLOQ value per data set is supported
      if (!isOfLength(value, 1)) {
        stop(messages$lloqOnlyScalar())
      }
      private$.yColumn$LLOQ <- toBaseUnit(
        quantityOrDimension = private$.yColumn$dimension,
        values = value,
        unit = private$.yColumn$displayUnit
      )
    },

    #' @field metaData Returns a named list of meta data defined for the data set.
    metaData = function(value) {
      if (missing(value)) {
        return(private$.dataRepository$metaData)
      }
      private$.throwPropertyIsReadonly("metaData")
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class.
    #' Either create a `DataSet` from a `DataRepository` (e.g. loaded from a PKML)
    #' or an empty `DataSet`. In case of an empty `DataSet`, a `name` must be
    #' provided.
    #'
    #' @param dataRepository Instance of the `DataRepository` object to wrap.
    #' If `NULL`, an empty `DataRepository` is created.
    #' @param name Name of the `DataSet` if created from scratch (no `dataRepository`)
    #' provided. Ignored if `dataRepository` is not `NULL`.
    #' @return A new `DataSet` object.
    initialize = function(name = NULL, dataRepository = NULL) {
      if (is.null(dataRepository) && is.null(name)) {
        stop(messages$errorDataSetNameMissing)
      }
      private$.dataRepository <- dataRepository %||% private$.createDataRepository()
      private$.initializeCache()

      # Set the name if no `dataRepository` provided
      if (is.null(dataRepository)) {
        self$name <- name
      }
    },

    #' @description
    #' Adds a new entry to meta data list or changes its value if the name is
    #' already present.
    #'
    #' @param name Name of new meta data list entry
    #' @param value Value of new meta data list entry
    addMetaData = function(name, value) {
      private$.dataRepository$addMetaData(name, value)
    },

    #' @description
    #' Removes the meta data entry in the list if one is defined with this name
    #'
    #' @param name Name of meta data entry to delete
    removeMetaData = function(name) {
      private$.dataRepository$removeMetaData(name)
    },

    #' @description
    #' Sets the xValues, yValues, and (optionally) yErrorValues into the dataSet.
    #' Note: xValues, yValues and yErrorValues must have the same length
    #' @param xValues xValues to use
    #' @param yValues yValues to use
    #' @param yErrorValues Optional error values associated with yValues
    setValues = function(xValues, yValues, yErrorValues = NULL) {
      validateIsNumeric(xValues)
      validateIsNumeric(yValues)
      validateIsNumeric(yErrorValues, nullAllowed = TRUE)
      validateIsSameLength(xValues, yValues)
      if (!is.null(yErrorValues)) {
        validateIsSameLength(xValues, yErrorValues)
      }

      private$.setColumnValues(column = private$.xColumn, xValues)
      private$.setColumnValues(column = private$.yColumn, yValues)

      # yError column must be removed in case yError is NULL and there is a
      # yErrorColumn already
      if (is.null(yErrorValues) && !is.null(private$.yErrorColumn)) {
        dataRepositoryTask <- .getNetTaskFromCache("DataRepositoryTask")
        dataRepositoryTask$call(
          "RemoveColumn",
          private$.dataRepository,
          private$.yErrorColumn
        )
        private$.yErrorColumn <- NULL
      }

      if (!is.null(yErrorValues)) {
        private$.createErrorColumnIfMissing()
        private$.setColumnValues(column = private$.yErrorColumn, yErrorValues)
      }
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      private$.printLine("Name", self$name)
      private$.printLine("X dimension", self$xDimension)
      private$.printLine("X unit", self$xUnit)
      private$.printLine("Y dimension", self$yDimension)
      private$.printLine("Y unit", self$yUnit)
      private$.printLine("Error type", self$yErrorType)
      private$.printLine("Error unit", self$yErrorUnit)
      private$.printLine("Molecular weight", self$molWeight)
      private$.printLine("LLOQ", self$LLOQ)
      private$.printLine("Meta data:")
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
      # values are set in the display unit. We need to make sure we convert them
      # to the base unit
      valuesInBaseUnit <- toBaseUnit(
        quantityOrDimension = column$dimension,
        values = values,
        unit = column$displayUnit
      )
      column$values <- valuesInBaseUnit
      invisible(self)
    },
    .getColumnValues = function(column) {
      # we need to convert the values in the display unit
      toUnit(
        quantityOrDimension = column$dimension,
        values = column$values,
        targetUnit = column$displayUnit
      )
    },
    .setColumnDimension = function(column, value) {
      # no need to update anything if we are setting the same values
      if (column$dimension == value) {
        return()
      }

      # save the values in their display unit before updating
      values <- private$.getColumnValues(column)

      # now we need to update dimension (display unit will be the default one as per .NET implementation)
      column$dimension <- value

      private$.setColumnValues(column, values)
    },
    .setColumnUnit = function(column, unit) {
      # no need to update anything if we are setting the same values
      if (column$displayUnit == unit) {
        return()
      }

      # save the values in their display unit before updating
      values <- private$.getColumnValues(column)

      # now we need to update dimension and display unit
      column$displayUnit <- unit

      private$.setColumnValues(column, values)
    },
    .setErrorType = function(errorType) {
      if (is.null(private$.yErrorColumn)) {
        return(invisible(self))
      }
      # If error type does not change, do nothing
      if (errorType == self$yErrorType) {
        return(invisible(self))
      }

      validateEnumValue(errorType, DataErrorType)
      column <- private$.yErrorColumn
      values <- private$.getColumnValues(column)
      dataInfo <- column$get("DataInfo")
      auxType <- rSharp::getStatic("OSPSuite.Core.Domain.Data.AuxiliaryType", errorType)
      dataInfo$set("AuxiliaryType", auxType)

      # Geometric to arithmetic - set to the same dimension and unit as yValues
      if (errorType == DataErrorType$ArithmeticStdDev) {
        private$.setColumnDimension(column, self$yDimension)
        private$.setColumnUnit(column, self$yUnit)
      }

      # Arithmetic to geometric - set to dimensionless
      if (errorType == DataErrorType$GeometricStdDev) {
        private$.setColumnDimension(column, ospDimensions$Dimensionless)
      }
      private$.setColumnValues(column, values)
      invisible(self)
    },
    .createDataRepository = function() {
      dataRepositoryTask <- .getNetTaskFromCache("DataRepositoryTask")
      dataRepository <- dataRepositoryTask$call("CreateEmptyObservationRepository", "xValues", "yValues")
      return(DataRepository$new(dataRepository))
    },
    .initializeCache = function() {
      dataRepositoryTask <- .getNetTaskFromCache("DataRepositoryTask")
      private$.xColumn <- private$.dataRepository$baseGrid

      netYColumn <- dataRepositoryTask$call("GetMeasurementColumn", private$.dataRepository)
      private$.yColumn <- DataColumn$new(netYColumn)
      netYErrorColumn <- dataRepositoryTask$call("GetErrorColumn", netYColumn)

      if (!is.null(netYErrorColumn)) {
        private$.yErrorColumn <- DataColumn$new(netYErrorColumn)
      }
    },
    .createErrorColumnIfMissing = function() {
      if (!is.null(private$.yErrorColumn)) {
        return()
      }

      dataRepositoryTask <- .getNetTaskFromCache("DataRepositoryTask")
      netYErrorColumn <- dataRepositoryTask$call(
        "AddErrorColumn",
        private$.yColumn,
        "yErrorValues",
        DataErrorType$ArithmeticStdDev
      )
      private$.yErrorColumn <- DataColumn$new(netYErrorColumn)
    }
  )
)
