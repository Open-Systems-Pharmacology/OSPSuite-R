#' @title DataImporterConfiguration
#' @docType class
#' @description Configuration of data import from excel or csv files. To be used with #TODO
#' @export
#' @format NULL
DataImporterConfiguration <- R6::R6Class(
  "DataSet",
  inherit = DotNetWrapper,
  cloneable = TRUE,
  active = list(
    #' @field timeColumn Name of the column for time values
    timeColumn = function(value) {
      column <- private$.timeColumn
      if (missing(value)) {
        return(rClr::clrGet(column, "ColumnName"))
      }
      validateIsString(value)
      rClr::clrSet(column, "ColumnName", enc2utf8(value))
    },

    #' @field timeUnit If \code{timeUnitFromColumn} is \code{FALSE}, unit of the values in time column
    #' If \code{timeUnitFromColumn} is \code{TRUE}, name of the column with units of the values in time column
    timeUnit = function(value) {
      column <- private$.timeColumn
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (self$timeUnitFromColumn) {
          return(rClr::clrGet(unit, "ColumnName"))
        }
        return(rClr::clrGet(unit, "SelectedUnit"))
      }
      validateIsString(value)
      # Fixed unit or from column?
      if (self$timeUnitFromColumn) {
        rClr::clrSet(unit, "ColumnName", enc2utf8(value))
      } else {
        validateUnit(enc2utf8(value), ospDimensions$Time)
        rClr::clrSet(unit, "SelectedUnit", enc2utf8(value))
      }
    },

    #' @field timeUnitFromColumn If \code{TRUE}, units of the values in time column
    #' are defined in the column \code{timeUnit}. If \code{FALSE}, the unit is defined by
    #' \code{timeUnit}.
    timeUnitFromColumn = function(value) {
      column <- private$.timeColumn
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        columnName <- rClr::clrGet(unit, "ColumnName")
        if (is.null(columnName) || nchar(columnName) == 0) {
          return(FALSE)
        }
        return(TRUE)
      }
      validateIsLogical(value)
      rClr::clrCall(private$.dataImporterTask, "SetIsUnitFromColumn", column, value)
    },

    #' @field measurementColumn Name of the column for measurement values
    measurementColumn = function(value) {
      column <- private$.measurementColumn
      if (missing(value)) {
        return(rClr::clrGet(column, "ColumnName"))
      }
      validateIsString(value)
      rClr::clrSet(column, "ColumnName", enc2utf8(value))
    },

    #' @field measurementDimension If \code{measurementUnitFromColumn} is \code{FALSE}, dimension of the values in measurement column
    #' If \code{measurementUnitFromColumn} is \code{TRUE}, the dimension is guessed from the unit defined in the column \code{measurementUnit} during import process and \code{$measurementDimension} is \code{NULL}.
    #' When changing dimension, the unit is set to the base unit of this dimension.
    measurementDimension = function(value) {
      column <- private$.measurementColumn
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (self$measurementUnitFromColumn) {
          return(NULL)
        }
        dimension <- rClr::clrGet(mappedColumn, "Dimension")
        return(rClr::clrGet(dimension, "DisplayName"))
      }
      validateIsString(value)
      # Fixed unit or from column?
      if (self$measurementUnitFromColumn) {
        # do nothing as it should be NULL
      } else {
        validateDimension(enc2utf8(value))
        rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(enc2utf8(value)))
        rClr::clrSet(unit, "SelectedUnit", getBaseUnit(enc2utf8(value)))

        # also change dimension of the error
        column <- private$.errorColumn
        if (!is.null(column)) {
          mappedColumn <- rClr::clrGet(column, "MappedColumn")
          unit <- rClr::clrGet(mappedColumn, "Unit")
          rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(enc2utf8(value)))
          rClr::clrSet(unit, "SelectedUnit", getBaseUnit(enc2utf8(value)))
        }
      }
    },

    #' @field measurementUnit If \code{measurementUnitFromColumn} is \code{FALSE}, unit of the values in measurement column
    #' If \code{measurementUnitFromColumn} is \code{TRUE}, name of the column with units of the values in measurement column
    measurementUnit = function(value) {
      column <- private$.measurementColumn
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (self$measurementUnitFromColumn) {
          return(rClr::clrGet(unit, "ColumnName"))
        }
        return(rClr::clrGet(unit, "SelectedUnit"))
      }
      validateIsString(value)
      # Fixed unit or from column?
      if (self$measurementUnitFromColumn) {
        rClr::clrSet(unit, "ColumnName", enc2utf8(value))
      } else {
        validateUnit(enc2utf8(value), self$measurementDimension)
        rClr::clrSet(unit, "SelectedUnit", enc2utf8(value))
      }
    },

    #' @field measurementUnitFromColumn If \code{TRUE}, units of the values in measurement column
    #' are defined in the column \code{measurementUnit}. If \code{FALSE}, the unit is defined by
    #' \code{measurementUnit}.
    measurementUnitFromColumn = function(value) {
      column <- private$.measurementColumn
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        columnName <- rClr::clrGet(unit, "ColumnName")
        if (is.null(columnName) || nchar(columnName) == 0) {
          return(FALSE)
        }
        return(TRUE)
      }
      validateIsLogical(value)
      rClr::clrCall(private$.dataImporterTask, "SetIsUnitFromColumn", column, value)
      # Also change isUnitFromColumn for error column
      if (!is.null(private$.errorColumn)) {
        rClr::clrCall(private$.dataImporterTask, "SetIsUnitFromColumn", private$.errorColumn, value)
      }
    },

    #' @field errorColumn Name of the column for measurement error values
    #' If no error column is defined, the value is \code{NULL}. Setting the value
    #' to \code{NULL} removes an existing error column.
    errorColumn = function(value) {
      column <- private$.errorColumn
      if (missing(value)) {
        if (is.null(column)) {
          return(NULL)
        }
        return(rClr::clrGet(column, "ColumnName"))
      }
      # If value is NULL, remove the error column
      if (is.null(value)) {
        rClr::clrCall(private$.dataImporterTask, "RemoveError", self$ref)
        private$.errorColumn <- NULL
      } else {
        validateIsString(value)
        # Create an error column if none is present in the configuration
        if (is.null(column)) {
          private$.addErrorColumn()
        }
        rClr::clrSet(private$.errorColumn, "ColumnName", enc2utf8(value))
      }
    },

    #' @field errorUnit If \code{measurementUnitFromColumn} is \code{FALSE}, unit of the values in the error column
    #' If \code{measurementUnitFromColumn} is \code{TRUE}, name of the column with units of the values in error column
    #' If no error column is present, the value is \code{NULL}
    errorUnit = function(value) {
      column <- private$.errorColumn

      if (is.null(column)) {
        if (missing(value)) {
          return(NULL)
        }
        return(invisible(self))
      }

      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (self$measurementUnitFromColumn) {
          return(rClr::clrGet(unit, "ColumnName"))
        }
        return(rClr::clrGet(unit, "SelectedUnit"))
      }
      validateIsString(value)
      # Fixed unit or from column?
      if (self$measurementUnitFromColumn) {
        rClr::clrSet(unit, "ColumnName", enc2utf8(value))
      } else {
        validateUnit(enc2utf8(value), self$measurementDimension)
        rClr::clrSet(unit, "SelectedUnit", enc2utf8(value))
      }
    },

    #' @field errorType Type of the measurement error values. See enum \code{DataErrorType}
    #' for possible values
    #' If no error column is present, the value is \code{NULL}
    errorType = function(value) {
      column <- private$.errorColumn

      if (is.null(column)) {
        if (missing(value)) {
          return(NULL)
        }
        return(invisible(self))
      }

      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      if (missing(value)) {
        errorType <- rClr::clrGet(mappedColumn, "ErrorStdDev")
        # The string returned must be mapped to the naming used in DataSet (resp. data repository)
        return(.ImporterErrorTypeToDataSetErrorType[[errorType]])
      }
      validateEnumValue(value, .ImporterErrorTypeToDataSetErrorType)
      rClr::clrSet(
        mappedColumn, "ErrorStdDev",
        getEnumKey(enum = .ImporterErrorTypeToDataSetErrorType, value)
      )
    },

    #' @field groupingColumns Column names by which the data will be grouped
    groupingColumns = function(value) {
      if (missing(value)) {
        return(rClr::clrCall(private$.dataImporterTask, "GetAllGroupingColumns", self$ref))
      }
      private$throwPropertyIsReadonly("groupingColumns")
    },

    #' @field sheets Names of the sheets (list of strings) of the excel workbook for which the
    #' configuration will be applied.
    sheets = function(value) {
      if (missing(value)) {
        return(rClr::clrCall(private$.dataImporterTask, "GetAllLoadedSheets", self$ref))
      }
      if (length(value) == 0) {
        rClr::clrCall(self$ref, "ClearLoadedSheets")
        return(invisible(self))
      }
      validateIsString(value)
      rClr::clrCall(private$.dataImporterTask, "SetAllLoadedSheet", self$ref, value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param configurationFilePath Path to the XML file with stored configuration
    #' (e.g. create in PK-Sim or MoBi).
    #' If \code{NULL} (default), an empty configuration with columns "Time" and
    #' "Measurement" is created.
    #' @return A new `DataImporterConfiguration` object.
    initialize = function(configurationFilePath = NULL) {
      importerTask <- getNetTask("DataImporterTask")

      if (is.null(configurationFilePath)) {
        ref <- rClr::clrCall(importerTask, "CreateConfiguration")
      } else {
        validateIsString(configurationFilePath)
        ref <- rClr::clrCall(importerTask, "GetConfiguration", configurationFilePath)
      }
      super$initialize(ref)
      private$.dataImporterTask <- importerTask

      private$.timeColumn <- rClr::clrCall(importerTask, "GetTime", ref)
      private$.measurementColumn <- rClr::clrCall(importerTask, "GetMeasurement", ref)
      private$.errorColumn <- rClr::clrCall(importerTask, "GetError", ref)
    },

    #' @description
    #' Save configuration to a XML file that can be used in PKSim/MoBi
    #' @param filePath Path (incl. file name) to the location where the configuration
    #' will be exported to.
    saveConfiguration = function(filePath) {
      validateIsString(filePath)
      filePath <- expandPath(filePath)

      rClr::clrCall(private$.dataImporterTask, "SaveConfiguration", self$ref, filePath)
      invisible(self)
    },

    #' @description
    #' Add a column for grouping the data sets
    #' @param column Name of the column
    addGroupingColumn = function(column) {
      validateIsString(column)
      rClr::clrCall(private$.dataImporterTask, "AddGroupingColumn", self$ref, enc2utf8(column))
      invisible(self)
    },

    #' @description
    #' Remove a column for grouping the data sets
    #' @param column Name of the column
    removeGroupingColumn = function(column) {
      validateIsString(column)
      rClr::clrCall(private$.dataImporterTask, "RemoveGroupingColumn", self$ref, enc2utf8(column))
      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("Time column", self$timeColumn)
      private$printLine("Time unit", self$timeUnit)
      private$printLine("Time unit from column", self$timeUnitFromColumn)
      private$printLine("Measurement column", self$measurementColumn)
      private$printLine("Measurement unit", self$measurementUnit)
      private$printLine("Measurement unit from column", self$measurementUnitFromColumn)
      private$printLine("Error column", self$errorColumn)
      private$printLine("Error type", self$errorType)
      private$printLine("Error unit", self$errorUnit)
      private$printLine("Grouping columns", self$groupingColumns)
      private$printLine("Sheets", self$sheets)

      invisible(self)
    }
  ),
  private = list(
    .dataImporterTask = NULL,
    .timeColumn = NULL,
    .measurementColumn = NULL,
    .errorColumn = NULL,
    .addErrorColumn = function() {
      rClr::clrCall(private$.dataImporterTask, "AddError", self$ref)
      column <- rClr::clrCall(private$.dataImporterTask, "GetError", self$ref)
      private$.errorColumn <- column

      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(enc2utf8(self$measurementDimension)))
      rClr::clrSet(unit, "SelectedUnit", enc2utf8(self$measurementUnit))
    }
  )
)

#' Mapping of string representation for the error types supported by DataSet
#' to the values supported in the importer configuration
#' @include enum.R
.ImporterErrorTypeToDataSetErrorType <- enum(c(
  "Arithmetic Standard Deviation" = "ArithmeticStdDev",
  "Geometric Standard Deviation" = "GeometricStdDev"
))