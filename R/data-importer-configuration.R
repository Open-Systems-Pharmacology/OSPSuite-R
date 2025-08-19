#' @title DataImporterConfiguration
#' @docType class
#' @description Configuration of data import from excel or csv files. To be used with #TODO
#' @export
#' @format NULL
DataImporterConfiguration <- R6::R6Class(
  "DataImporterConfiguration",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  active = list(
    #' @field timeColumn Name of the column for time values
    timeColumn = function(value) {
      column <- private$.timeColumn()
      if (missing(value)) {
        return(column$get("ColumnName"))
      }
      validateIsString(value)
      column$set("ColumnName", value)
    },

    #' @field timeUnit If `isTimeUnitFromColumn` is `FALSE`, unit of the values
    #'   in time column If `isTimeUnitFromColumn` is `TRUE`, name of the column
    #'   with units of the values in time column.
    timeUnit = function(value) {
      column <- private$.timeColumn()
      mappedColumn <- column$get("MappedColumn")
      unit <- mappedColumn$get("Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(unit$get("ColumnName"))
        }
        return(unit$get("SelectedUnit"))
      }
      validateIsString(value)
      private$.setColumnUnit(column = column, value = value)
    },

    #' @field isTimeUnitFromColumn If `TRUE`, units of the values in time column
    #' are defined in the column `timeUnit`. If `FALSE`, the unit is defined by
    #' the value of `timeUnit`.
    isTimeUnitFromColumn = function(value) {
      column <- private$.timeColumn()
      if (missing(value)) {
        return(private$.isUnitFromColumn(column))
      }
      validateIsLogical(value)
      private$.dataImporterTask$call("SetIsUnitFromColumn", column, value)
    },

    #' @field measurementColumn Name of the column for measurement values
    measurementColumn = function(value) {
      column <- private$.measurementColumn()
      if (missing(value)) {
        return(column$get("ColumnName"))
      }
      validateIsString(value)
      column$set("ColumnName", value)
    },

    #' @field lloqColumn Name of the column for LLOQ values
    #' If the column name is not set (value `NULL`), LLOQ values
    #' will be imported from the measurement column if values are written in the form '< xxx' (e.g., '<0.001').
    #' Otherwise, the values will be imported from the specified column
    lloqColumn = function(value) {
      column <- private$.measurementColumn()$get("MappedColumn")
      if (missing(value)) {
        return(column$get("LloqColumn"))
      }
      validateIsString(value, nullAllowed = TRUE)
      column$set("LloqColumn", value)
    },

    #' @field measurementDimension If `isMeasurementUnitFromColumn` is `FALSE`,
    #'   dimension of the values in measurement column If
    #'   `isMeasurementUnitFromColumn` is `TRUE`, the dimension is guessed from
    #'   the unit defined in the column `measurementUnit` during import process
    #'   and `$measurementDimension` is `NULL`. When changing dimension, the
    #'   unit is set to the base unit of this dimension.
    measurementDimension = function(value) {
      column <- private$.measurementColumn()
      mappedColumn <- column$get("MappedColumn")
      unit <- mappedColumn$get("Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(NULL)
        }
        dimension <- mappedColumn$get("Dimension")
        return(ifNotNull(dimension, dimension$get("DisplayName")))
      }
      validateIsString(value)
      # Fixed unit or from column?
      if (private$.isUnitFromColumn(column)) {
        # do nothing as it should be NULL
        return(invisible(self))
      }
      validateDimension(value)
      mappedColumn$set("Dimension", getDimensionByName(value))
      unit$set("SelectedUnit", getBaseUnit(value))

      # also change dimension of the error
      column <- private$.errorColumn()
      if (!is.null(column)) {
        mappedColumn <- column$get("MappedColumn")
        unit <- mappedColumn$get("Unit")
        mappedColumn$set("Dimension", getDimensionByName(value))
        private$.setColumnUnit(column, getBaseUnit(value))
      }
    },

    #' @field measurementUnit If `isMeasurementUnitFromColumn` is `FALSE`, unit of the values in measurement column
    #' If `isMeasurementUnitFromColumn` is `TRUE`, name of the column with units of the values in measurement column
    measurementUnit = function(value) {
      column <- private$.measurementColumn()
      mappedColumn <- column$get("MappedColumn")
      unit <- mappedColumn$get("Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(unit$get("ColumnName"))
        }
        return(unit$get("SelectedUnit"))
      }
      validateIsString(value)
      private$.setColumnUnit(column = column, value = value)
    },

    #' @field isMeasurementUnitFromColumn If `TRUE`, units of the values in measurement column
    #' are defined in the column `measurementUnit`. If `FALSE`, the unit is defined by
    #' the value of `measurementUnit`.
    isMeasurementUnitFromColumn = function(value) {
      column <- private$.measurementColumn()
      if (missing(value)) {
        return(private$.isUnitFromColumn(column))
      }
      validateIsLogical(value)
      private$.dataImporterTask$call("SetIsUnitFromColumn", column, value)
      # Also change isUnitFromColumn for error column
      if (!is.null(private$.errorColumn())) {
        private$.dataImporterTask$call("SetIsUnitFromColumn", private$.errorColumn(), value)
      }
    },

    #' @field errorColumn Name of the column for measurement error values
    #' If no error column is defined, the value is `NULL`. Setting the value
    #' to `NULL` removes an existing error column.
    errorColumn = function(value) {
      column <- private$.errorColumn()
      if (missing(value)) {
        return(ifNotNull(column, column$get("ColumnName")))
      }
      # If value is NULL, remove the error column
      if (is.null(value)) {
        private$.dataImporterTask$call("RemoveError", self)
      } else {
        validateIsString(value)
        # Create an error column if none is present in the configuration
        if (is.null(column)) {
          private$.addErrorColumn()
        }
        private$.errorColumn()$set("ColumnName", value)
      }
    },

    #' @field errorUnit If `isMeasurementUnitFromColumn` is `FALSE`, unit of the values in the error column
    #' If `isMeasurementUnitFromColumn` is `TRUE`, name of the column with units of the values in error column
    #' If no error column is present, the value is `NULL`
    errorUnit = function(value) {
      column <- private$.errorColumn()
      if (is.null(column)) {
        if (missing(value)) {
          return(NULL)
        }
        return(invisible(self))
      }

      mappedColumn <- column$get("MappedColumn")
      unit <- mappedColumn$get("Unit")
      if (missing(value)) {
        # Fixed unit or from column?
        if (private$.isUnitFromColumn(column)) {
          return(unit$get("ColumnName"))
        }
        return(unit$get("SelectedUnit"))
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

      mappedColumn <- column$get("MappedColumn")
      if (missing(value)) {
        errorType <- mappedColumn$get("ErrorStdDev")
        # The string returned must be mapped to the naming used in DataSet (resp. data repository)
        return(.ImporterErrorTypeToDataSetErrorType[[errorType]])
      }
      validateEnumValue(value, .ImporterErrorTypeToDataSetErrorType)
      mappedColumn$set(
        "ErrorStdDev",
        getEnumKey(enum = .ImporterErrorTypeToDataSetErrorType, value)
      )
    },

    #' @field groupingColumns Column names by which the data will be grouped
    groupingColumns = function(value) {
      if (missing(value)) {
        return(private$.dataImporterTask$call("GetAllGroupingColumns", self))
      }
      private$.throwPropertyIsReadonly("groupingColumns")
    },

    #' @field sheets Names of the sheets (list of strings) of the excel workbook for which the
    #' configuration will be applied.
    sheets = function(value) {
      if (missing(value)) {
        return(private$.dataImporterTask$call("GetAllLoadedSheets", self))
      }
      if (length(value) == 0) {
        self$call("ClearLoadedSheets")
        return(invisible(self))
      }
      validateIsString(value)
      private$.dataImporterTask$call("SetAllLoadedSheet", self, value)
    },
    #' @field namingPattern Regular expression used for naming of loaded data sets.
    #' Words between curly brackets (e.g. `{Group Id}`) will be replaced by the value
    #' in the corresponding column. Further keywords are `{Source}` for the file name
    #' and `{Sheet}` for sheet name.
    namingPattern = function(value) {
      if (missing(value)) {
        pattern <- self$get("NamingConventions")
        # Create a default pattern if no is defined
        if (is.null(pattern)) {
          pattern <- "{Source}.{Sheet}"
          self$set("NamingConventions", pattern)
        }
        return(pattern)
      }
      validateIsString(value)
      self$set("NamingConventions", value)
    }
  ),
  public = list(
    #' @param netObject A `NetObject` with the reference to .NET DataImporterConfiguration object
    #' If `NULL` (default), an empty configuration with columns "Time" and
    #' "Measurement" is created.
    #' @description
    #' Initialize a new instance of the class
    #' @return A new `DataImporterConfiguration` object.
    initialize = function(netObject = NULL) {
      importerTask <- .getCoreTaskFromCache("DataImporterTask")
      if (is.null(netObject)) {
        netObject <- importerTask$call("CreateConfiguration")
      }
      super$initialize(netObject)
      private$.dataImporterTask <- importerTask

      # set timeColumn dimension and unit to default ("Time" and "h") if it is
      # not set properyl in the .NET object. This could happen when creating
      # a configuration for a xls-sheet and the unit could not be recognized.
      # Because the user cannot set the Dimension if time values, this must be
      # done during initialization phase.
      if (self$timeUnit == "?") {
        column <- importerTask$call("GetTime", netObject)
        mappedColumn <- column$get("MappedColumn")
        mappedColumn$set("Dimension", getDimensionByName(ospDimensions$Time))
        self$timeUnit <- ospUnits$Time$h
      }
    },

    #' @description
    #' Save configuration to a XML file that can be used in PK-Sim/MoBi
    #' @param filePath Path (incl. file name) to the location where the configuration
    #' will be exported to.
    saveConfiguration = function(filePath) {
      validateIsString(filePath)
      filePath <- .expandPath(filePath)

      private$.dataImporterTask$call("SaveConfiguration", self, filePath)
      invisible(self)
    },

    #' @description
    #' Add a column for grouping the data sets
    #' @param column Name of the column
    addGroupingColumn = function(column) {
      validateIsString(column)
      private$.dataImporterTask$call("AddGroupingColumn", self, column)
      invisible(self)
    },

    #' @description
    #' Remove a column for grouping the data sets
    #' @param column Name of the column
    removeGroupingColumn = function(column) {
      validateIsString(column)
      private$.dataImporterTask$call("RemoveGroupingColumn", self, column)
      invisible(self)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(
        list(
          "Time column" = self$timeColumn,
          "Time unit" = self$timeUnit,
          "Time unit from column" = self$isTimeUnitFromColumn,
          "Measurement column" = self$measurementColumn,
          "Measurement unit" = self$measurementUnit,
          "Measurement unit from column" = self$isMeasurementUnitFromColumn,
          "LLOQ column" = self$lloqColumn,
          "Error column" = self$errorColumn,
          "Error type" = self$errorType,
          "Error unit" = self$errorUnit,
          "Grouping columns" = self$groupingColumns,
          "Sheets" = self$sheets,
          "Naming pattern" = self$namingPattern
        ),
        print_empty = TRUE
      )
    }
  ),
  private = list(
    .dataImporterTask = NULL,
    .timeColumn = function() {
      return(private$.dataImporterTask$call("GetTime", self))
    },
    .measurementColumn = function() {
      return(private$.dataImporterTask$call("GetMeasurement", self))
    },
    .errorColumn = function() {
      return(private$.dataImporterTask$call("GetError", self))
    },
    .addErrorColumn = function() {
      private$.dataImporterTask$call("AddError", self)
      mappedColumn <- private$.errorColumn()$get("MappedColumn")
      mappedColumn$set("Dimension", getDimensionByName(self$measurementDimension))
    },
    .setColumnUnit = function(column, value) {
      mappedColumn <- column$get("MappedColumn")
      dimension <- mappedColumn$get("Dimension")
      # Fixed unit or from column?
      if (private$.isUnitFromColumn(column)) {
        # Get the old unit and set it as default unit
        unit <- mappedColumn$get("Unit")
        unitDescription <- rSharp::newObjectFromName("OSPSuite.Core.Import.UnitDescription", unit$get("SelectedUnit"), value)
      } else {
        validateUnit(value, dimension$get("Name"))
        unitDescription <- rSharp::newObjectFromName("OSPSuite.Core.Import.UnitDescription", value)
      }
      mappedColumn$set("Unit", unitDescription)
    },
    .isUnitFromColumn = function(column) {
      mappedColumn <- column$get("MappedColumn")
      unit <- mappedColumn$get("Unit")
      columnName <- unit$get("ColumnName")
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
