#' Transforms a single .NET object  or a list of .NET Object to their corresponding wrapper class in R.
#' Note that if the object is a single object, NULL will be returned if the .NET object is null. This allows semantic equivalence between .NET and R
#'
#' @param netObject The .NET object instances (single or list) to wrap
#' @param class The class definition that will be used to convert the parameter
#'
#' @return The wrapped object (single or list)
#' @keywords internal
toObjectType <- function(netObject, class) {
  if (!is.list(netObject)) {
    return(ospsuite.utils::ifNotNull(netObject, class$new(ref = netObject)))
  }
  sapply(c(netObject), function(x) {
    class$new(ref = x)
  })
}


#' This is required to ensure that we have no issue using the mu symbol in different OS
#' See https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/476 for details
#' @param unit Unit to encode
#' @import stringr
#' @keywords internal
encodeUnit <- function(unit) {
  mu <- ospsuiteEnv$muSymbol
  unit <- enc2utf8(unit)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xce, 0xbc))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(c(0xc2, 0xb5))), mu)
  unit <- str_replace(unit, rawToChar(as.raw(0xb5)), mu)
}


#' Retrieves the name of the constant in the specified enumeration that has the specified value.
#'
#' @inheritParams rClr::clrGetEnumNames
#' @param enumValue The value of a particular enumerated constant in terms of its underlying type. Typically an integer.
#'
#' @return A string containing the name of the enumerated constant in enumType whose value is enumValue; or null if no such constant is found.
#' @keywords internal
netEnumName <- function(enumType, enumValue) {
  netTypeObj <- rClr::clrGetType(enumType)
  rClr::clrCallStatic("System.Enum", methodName = "GetName", netTypeObj, enumValue)
}


#' Create a DataImporterConfiguration object for an XLS sheet
#' @details The function tries to parse the structure of the excel sheet and
#' creates a default configuration for this sheet. It is advised to check the
#' configuration and adjust if necessary before using with `loadDataSetsFromExcel()`.
#'
#' @param filePath Path to XLS file
#' @param sheet optional - name of the sheet. If no sheet is specified, the first
#' sheet of the XLS file is used.
#'
#' @return `DataImporterConfiguration` object for XLS file to be used in
#' `loadDataSetsFromExcel()`.
#' @examples
#' xlsFilePath <- "../CompiledDataSet.xlsx"
#' importerConfiguration <- createConfigurationForFile(xlsFilePath)
#' importerConfiguration$sheets <- "TestSheet_1"
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfiguration = importerConfiguration,
#'   importAllSheets = TRUE
#' )
#' @export
createConfigurationForFile = function(filePath, sheet = NULL) {
  # warum sind aenderungen, z.b. unit nicht in ref?????
  validateIsString(filePath)

  importerConfiguration <- DataImporterConfiguration$new()
  importerTask <- getNetTask("DataImporterTask")
  if(is.null(sheet)) {
    ref <- rClr::clrCall(importerTask, "CreateConfigurationFor", filePath)
  } else {
    ref <- rClr::clrCall(importerTask, "CreateConfigurationFor", filePath, sheet)
  }
  importerConfiguration$ref <- ref

  measurement <- rClr::clrCall(importerTask, "GetMeasurement", ref)
  importerConfiguration$measurementColumn <- rClr::clrGet(measurement, "ColumnName")
  mappedMeasurementCol <- rClr::clrGet(measurement, "MappedColumn")
  measurementUnitDescription <- rClr::clrGet(mappedMeasurementCol, "Unit")
  measurementUnit <- rClr::clrGet(measurementUnitDescription, "SelectedUnit")
  # measurementUnitCol <- rClr::clrGet(measurementUnitDescription, "ColumnName")
  dimension <- rClr::clrGet(mappedMeasurementCol, "Dimension")

  # set to default if unit and dimension can not be read from file, i.e. it is "?"
  if (measurementUnit == "?") {
    rClr::clrSet(mappedMeasurementCol, "Dimension", getDimensionByName("Concentration (molar)"))
    rClr::clrSet(measurementUnitDescription, "SelectedUnit", getBaseUnit("Concentration (molar)"))
  } else {
  # if (is.null(measurementUnitCol)) {
    if (is.null(dimension)) {
      importerConfiguration$measurementDimension <- getDimensionForUnit(measurementUnit)
    } else {
      importerConfiguration$measurementDimension <- dimension
    }
    importerConfiguration$measurementUnit <- measurementUnit
  # } else {
  #   importerConfiguration$measurementUnitFromColumn <- TRUE
  # }
  }

  time <- rClr::clrCall(importerTask, "GetTime", ref)
  importerConfiguration$timeColumn <- rClr::clrGet(time, "ColumnName")
  mappedTimeCol <- rClr::clrGet(time, "MappedColumn")
  timeUnitDescription <- rClr::clrGet(mappedTimeCol, "Unit")
  importerConfiguration$timeUnit <- rClr::clrGet(timeUnitDescription, "SelectedUnit")
  timeUnitCol <- rClr::clrGet(timeUnitDescription, "ColumnName")
  if(!is.null(timeUnitCol)) {
    importerConfiguration$timeUnitFromColumn <- TRUE
  }

  error <- rClr::clrCall(importerTask, "GetError", ref)
  if(!is.null(error)) {
    importerConfiguration$errorColumn <- rClr::clrGet(error, "ColumnName")
    mappedErrorCol <- rClr::clrGet(error, "MappedColumn")
    importerConfiguration$errorType <- .ImporterErrorTypeToDataSetErrorType[[rClr::clrGet(mappedErrorCol, "ErrorStdDev")]]
    errorUnitDescription <- rClr::clrGet(mappedErrorCol, "Unit")
    importerConfiguration$errorUnit <- rClr::clrGet(errorUnitDescription, "SelectedUnit")
  }

  return(importerConfiguration)
}
