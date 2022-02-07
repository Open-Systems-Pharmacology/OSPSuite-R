#' @title DataImporterConfiguration
#' @docType class
#' @description Configuration of data import from excel or csv files. To be used with #TODO
#' @export
#' @format NULL
DataImporterConfiguration <- R6::R6Class(
  "DataImporterConfiguration",
  inherit = DotNetWrapper,
  cloneable = TRUE,
  active = list(
    #' @field ref Underlying .NET reference
    ref = function(value) {
      if (missing(value)){
        return(private$.ref)
      }
      private$.ref <- value
    },

    #' @field timeColumn Name of the column for time values
    timeColumn = function(value) {
      column <- private$.timeColumn()
      if (missing(value)) {
        return(rClr::clrGet(column, "ColumnName"))
      }
      validateIsString(value)
      rClr::clrSet(column, "ColumnName", enc2utf8(value))
    },

    #' @field timeUnit If `timeUnitFromColumn` is `FALSE`, unit of the values in time column
    #' If `timeUnitFromColumn` is `TRUE`, name of the column with units of the values in time column
    timeUnit = function(value) {
      column <- private$.timeColumn()
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(rClr::clrGet(unit, "ColumnName"))
        }
        return(rClr::clrGet(unit, "SelectedUnit"))
      }
      validateIsString(value)
      private$.setColumnUnit(column = column, value = value)
    },

    #' @field timeUnitFromColumn If `TRUE`, units of the values in time column
    #' are defined in the column `timeUnit`. If `FALSE`, the unit is defined by
    #' `timeUnit`.
    timeUnitFromColumn = function(value) {
      column <- private$.timeColumn()
      if (missing(value)) {
        return(private$.isUnitFromColumn(column))
      }
      validateIsLogical(value)
      rClr::clrCall(private$.dataImporterTask, "SetIsUnitFromColumn", column, value)
    },

    #' @field measurementColumn Name of the column for measurement values
    measurementColumn = function(value) {
      column <- private$.measurementColumn()
      if (missing(value)) {
        return(rClr::clrGet(column, "ColumnName"))
      }
      validateIsString(value)
      rClr::clrSet(column, "ColumnName", enc2utf8(value))
    },

    #' @field measurementDimension If `measurementUnitFromColumn` is `FALSE`, dimension of the values in measurement column
    #' If `measurementUnitFromColumn` is `TRUE`, the dimension is guessed from the unit defined in the column `measurementUnit` during import process and `$measurementDimension` is `NULL`.
    #' When changing dimension, the unit is set to the base unit of this dimension.
    measurementDimension = function(value) {
      column <- private$.measurementColumn()
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(NULL)
        }
        dimension <- rClr::clrGet(mappedColumn, "Dimension")
        return(ospsuite.utils::ifNotNull(dimension, rClr::clrGet(dimension, "DisplayName")))
      }
      validateIsString(value)
      # Fixed unit or from column?
      if (private$.isUnitFromColumn(column)) {
        # do nothing as it should be NULL
        return(invisible(self))
      }
      value <- enc2utf8(value)
      validateDimension(value)
      rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(value))
      rClr::clrSet(unit, "SelectedUnit", getBaseUnit(value))

      # also change dimension of the error
      column <- private$.errorColumn()
      if (!is.null(column)) {
        mappedColumn <- rClr::clrGet(column, "MappedColumn")
        unit <- rClr::clrGet(mappedColumn, "Unit")
        rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(value))
        private$.setColumnUnit(column, getBaseUnit(value))
      }
    },

    #' @field measurementUnit If `measurementUnitFromColumn` is `FALSE`, unit of the values in measurement column
    #' If `measurementUnitFromColumn` is `TRUE`, name of the column with units of the values in measurement column
    measurementUnit = function(value) {
      column <- private$.measurementColumn()
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(rClr::clrGet(unit, "ColumnName"))
        }
        return(rClr::clrGet(unit, "SelectedUnit"))
      }
      validateIsString(value)
      private$.setColumnUnit(column = column, value = value)
    },

    #' @field measurementUnitFromColumn If `TRUE`, units of the values in measurement column
    #' are defined in the column `measurementUnit`. If `FALSE`, the unit is defined by
    #' `measurementUnit`.
    measurementUnitFromColumn = function(value) {
      column <- private$.measurementColumn()
      if (missing(value)) {
        return(private$.isUnitFromColumn(column))
      }
      validateIsLogical(value)
      rClr::clrCall(private$.dataImporterTask, "SetIsUnitFromColumn", column, value)
      # Also change isUnitFromColumn for error column
      if (!is.null(private$.errorColumn())) {
        rClr::clrCall(private$.dataImporterTask, "SetIsUnitFromColumn", private$.errorColumn(), value)
      }
    },

    #' @field errorColumn Name of the column for measurement error values
    #' If no error column is defined, the value is `NULL`. Setting the value
    #' to `NULL` removes an existing error column.
    errorColumn = function(value) {
      column <- private$.errorColumn()
      if (missing(value)) {
        return(ifNotNull(column, rClr::clrGet(column, "ColumnName")))
      }
      # If value is NULL, remove the error column
      if (is.null(value)) {
        rClr::clrCall(private$.dataImporterTask, "RemoveError", self$ref)
      } else {
        validateIsString(value)
        # Create an error column if none is present in the configuration
        if (is.null(column)) {
          private$.addErrorColumn()
        }
        rClr::clrSet(private$.errorColumn(), "ColumnName", enc2utf8(value))
      }
    },

    #' @field errorUnit If `measurementUnitFromColumn` is `FALSE`, unit of the values in the error column
    #' If `measurementUnitFromColumn` is `TRUE`, name of the column with units of the values in error column
    #' If no error column is present, the value is `NULL`
    errorUnit = function(value) {
      column <- private$.errorColumn()
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
        if (private$.isUnitFromColumn(column)) {
          return(rClr::clrGet(unit, "ColumnName"))
        }
        return(rClr::clrGet(unit, "SelectedUnit"))
      }
      validateIsString(value)
      private$.setColumnUnit(column = column, value = value)
    },

    #' @field errorType Type of the measurement error values. See enum `DataErrorType`
    #' for possible values
    #' If no error column is present, the value is `NULL`
    errorType = function(value) {
      column <- private$.errorColumn()

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
    },
    #' @field namingPattern Regular expression used for naming of loaded data sets.
    #' Words between curly brackets (e.g. \code{{Group Id}}) will be replaced by the value
    #' in the corresponding column. Further keywords are \code{{Source}} for the file name
    #' and \code{{Sheet}} for sheet name.
    namingPattern = function(value) {
      if (missing(value)) {
        pattern <- rClr::clrGet(self$ref, "NamingConventions")
        # Create a default pattern if no is defined
        if (is.null(pattern)) {
          pattern <- "{Source}.{Sheet}"
          rClr::clrSet(self$ref, "NamingConventions", pattern)
        }
        return(pattern)
      }
      validateIsString(value)
      rClr::clrSet(self$ref, "NamingConventions", enc2utf8(value))
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param configurationFilePath Path to the XML file with stored configuration
    #' (e.g. create in PK-Sim or MoBi).
    #' If `NULL` (default), an empty configuration with columns "Time" and
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
      private$printLine("Naming pattern", self$namingPattern)

      invisible(self)
    }
  ),
  private = list(
    .dataImporterTask = NULL,
    .timeColumn = function() {
      return(rClr::clrCall(private$.dataImporterTask, "GetTime", self$ref))
    },
    .measurementColumn = function() {
      return(rClr::clrCall(private$.dataImporterTask, "GetMeasurement", self$ref))
    },
    .errorColumn = function() {
      return(rClr::clrCall(private$.dataImporterTask, "GetError", self$ref))
    },
    .addErrorColumn = function() {
      rClr::clrCall(private$.dataImporterTask, "AddError", self$ref)
      mappedColumn <- rClr::clrGet(private$.errorColumn(), "MappedColumn")
      rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(enc2utf8(self$measurementDimension)))
    },
    .setColumnUnit = function(column, value) {
      value <- enc2utf8(value)
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      dimension <- rClr::clrGet(mappedColumn, "Dimension")
      # Fixed unit or from column?
      if (private$.isUnitFromColumn(column)) {
        # Get the old unit and set it as default unit
        unit <- rClr::clrGet(mappedColumn, "Unit")
        unitDescription <- rClr::clrNew("OSPSuite.Core.Import.UnitDescription", enc2utf8(rClr::clrGet(unit, "SelectedUnit")), value)
      } else {
        validateUnit(value, rClr::clrGet(dimension, "Name"))
        unitDescription <- rClr::clrNew("OSPSuite.Core.Import.UnitDescription", value)
      }
      rClr::clrSet(mappedColumn, "Unit", unitDescription)
    },
    .isUnitFromColumn = function(column) {
      mappedColumn <- rClr::clrGet(column, "MappedColumn")
      unit <- rClr::clrGet(mappedColumn, "Unit")
      columnName <- rClr::clrGet(unit, "ColumnName")
      return(!(is.null(columnName) || nchar(columnName) == 0))
    }
  )
)

#' Mapping of string representation for the error types supported by DataSet
#' to the values supported in the importer configuration
#'
#' @keywords internal
.ImporterErrorTypeToDataSetErrorType <- enum(c(
  "Arithmetic Standard Deviation" = "ArithmeticStdDev",
  "Geometric Standard Deviation" = "GeometricStdDev"
))
