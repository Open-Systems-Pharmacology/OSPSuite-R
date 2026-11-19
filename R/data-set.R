#' @title DataSet
#' @docType class
#' @description A class for storage of numerical x- and y-value pairs and
#'   optional error for y-values.
#' @export
#' @format NULL
DataSet <- R6::R6Class(
  "DataSet",
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
        return(.netEnumName(
          "OSPSuite.Core.Domain.Data.AuxiliaryType",
          errorTypeEnumVal
        ))
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
        return(toUnit(
          quantityOrDimension = ospDimensions$`Molecular weight`,
          values = molWeight,
          targetUnit = ospUnits$`Molecular weight`$`g/mol`
        ))
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

        return(toUnit(
          quantityOrDimension = private$.yColumn$dimension,
          values = lloq,
          targetUnit = private$.yColumn$displayUnit
        ))
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

    #' @field subjectCount Number of subjects from which the y values and the
    #' error values are calculated, e.g. for aggregated data reported as mean
    #' and standard deviation. `NULL` if not, set it to `NULL` or `NA` to remove it.
    subjectCount = function(value) {
      if (missing(value)) {
        return(.subjectCountFromDataRepository(
          private$.dataRepository,
          self$name
        ))
      }

      # A factor, e.g. from `read.csv(stringsAsFactors = TRUE)`, is used by
      # its label, not by its internal code
      if (is.factor(value)) {
        value <- as.character(value)
      }
      # `NaN` is not a missing value here but an invalid count, e.g. from a
      # failed calculation, so it must not remove an existing count
      isNaN <- is.numeric(value) && length(value) == 1 && is.nan(value)
      if (is.null(value) || (length(value) == 1 && is.na(value) && !isNaN)) {
        self$removeMetaData(.subjectCountMetaDataName)
        return(invisible(self))
      }
      # R numbers are doubles and text is accepted, but .NET needs an integer,
      # so the value is checked and converted here
      count <- .parseSubjectCount(value)
      if (is.na(count)) {
        stop(messages$errorInvalidSubjectCount(value))
      }
      private$.dataRepository$setSubjectCount(count)
      invisible(self)
    },

    #' @field outputPath Path of the simulation output that the observed data
    #' belongs to, e.g. `"Organism|PeripheralVenousBlood|Aciclovir|Plasma
    #' (Peripheral Venous Blood)"`, or `NULL` if not set. It is stored as the
    #' meta data entry `OutputPath`. When the data set is used in PK-Sim or
    #' MoBi version 13 or later, it is mapped automatically to the output with
    #' exactly this path. The path must match exactly, including upper and
    #' lower case. Set it to `NULL`, `NA`, or `""` to remove it. A path of
    #' only spaces is treated as not set.
    outputPath = function(value) {
      if (missing(value)) {
        path <- self$metaData[[.outputPathMetaDataName]]
        if (is.null(path) || length(path) != 1 || is.na(path)) {
          return(NULL)
        }
        path <- .outputPathAsText(path)
        # PK-Sim and MoBi treat an empty entry or an entry of only spaces as
        # not set
        if (.isBlankOutputPath(path)) {
          return(NULL)
        }
        return(path)
      }

      # A factor, e.g. from `read.csv(stringsAsFactors = TRUE)`, is used by
      # its label, not by its internal code
      if (is.factor(value)) {
        value <- as.character(value)
      }
      if (is.null(value) || (length(value) == 1 && is.na(value))) {
        self$removeMetaData(.outputPathMetaDataName)
        return(invisible(self))
      }
      if (!is.character(value) || length(value) != 1) {
        stop(messages$errorInvalidOutputPath(value))
      }
      if (.isBlankOutputPath(value)) {
        self$removeMetaData(.outputPathMetaDataName)
        return(invisible(self))
      }
      private$.dataRepository$addMetaData(.outputPathMetaDataName, value)
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
      private$.dataRepository <- dataRepository %||%
        private$.createDataRepository()
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
      if (identical(name, .subjectCountMetaDataName)) {
        # Same rules as for all other entries: the value is text. A missing
        # value does not remove the entry here, use `removeMetaData()`.
        validateIsString(value)
        if (is.na(.parseSubjectCount(value))) {
          stop(messages$errorInvalidSubjectCount(value))
        }
        self$subjectCount <- value
        return(invisible(self))
      }
      if (identical(name, .outputPathMetaDataName)) {
        # Like all other entries, the value must be one text value. A missing
        # value does not remove the entry here, use `removeMetaData()` or
        # `outputPath <- NULL`. A blank value is stored as given, and
        # `outputPath` treats it as not set.
        if (!is.character(value) || length(value) != 1 || is.na(value)) {
          stop(messages$errorInvalidOutputPath(value))
        }
      }
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
        negative <- !is.na(yErrorValues) & yErrorValues < 0
        if (any(negative)) {
          warning(messages$yErrorValuesNegative(sum(negative)))
          yErrorValues[negative] <- NA_real_
        }
      }

      private$.setColumnValues(column = private$.xColumn, xValues)
      private$.setColumnValues(column = private$.yColumn, yValues)

      # yError column must be removed in case yError is NULL and there is a
      # yErrorColumn already
      if (is.null(yErrorValues) && !is.null(private$.yErrorColumn)) {
        dataRepositoryTask <- .getCoreTaskFromCache("DataRepositoryTask")
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
      ospsuite.utils::ospPrintClass(self)
      outputPath <- self$outputPath
      items <- list(
        "Name" = self$name,
        "X dimension" = self$xDimension,
        "X unit" = self$xUnit,
        "Y dimension" = self$yDimension,
        "Y unit" = self$yUnit,
        "Error type" = self$yErrorType,
        "Error unit" = self$yErrorUnit,
        "Molecular weight" = self$molWeight,
        "LLOQ" = self$LLOQ,
        "Subject count" = self$subjectCount
      )
      # The output path is printed only if set
      if (!is.null(outputPath)) {
        items[["Output path"]] <- outputPath
      }
      ospsuite.utils::ospPrintItems(items, print_empty = TRUE)
      # The number of subjects and the output path are printed above, not
      # again as meta data. An output path entry that is not set, e.g. one of
      # only spaces, stays in the meta data list.
      printedAbove <- .subjectCountMetaDataName
      if (!is.null(outputPath)) {
        printedAbove <- c(printedAbove, .outputPathMetaDataName)
      }
      metaData <- self$metaData
      metaData <- metaData[!names(metaData) %in% printedAbove]
      ospsuite.utils::ospPrintItems(metaData, title = "Meta data")
    }
  ),
  private = list(
    .dataRepository = NULL,
    .xColumn = NULL,
    .yColumn = NULL,
    .yErrorColumn = NULL,
    .throwPropertyIsReadonly = function(propertyName) {
      stop(messages$errorPropertyReadOnly(propertyName))
    },
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
      auxType <- rSharp::getStatic(
        "OSPSuite.Core.Domain.Data.AuxiliaryType",
        errorType
      )
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
      dataRepositoryTask <- .getCoreTaskFromCache("DataRepositoryTask")
      dataRepository <- dataRepositoryTask$call(
        "CreateEmptyObservationRepository",
        "xValues",
        "yValues"
      )
      return(DataRepository$new(dataRepository))
    },
    .initializeCache = function() {
      dataRepositoryTask <- .getCoreTaskFromCache("DataRepositoryTask")
      private$.xColumn <- private$.dataRepository$baseGrid

      netYColumn <- dataRepositoryTask$call(
        "GetMeasurementColumn",
        private$.dataRepository
      )
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

      dataRepositoryTask <- .getCoreTaskFromCache("DataRepositoryTask")
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
