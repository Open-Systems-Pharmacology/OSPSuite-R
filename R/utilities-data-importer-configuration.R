#' Create a `DataImporterConfiguration` for an XLS sheet
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
#' xlsFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
#' importerConfiguration <- createImporterConfigurationForFile(xlsFilePath)
#' importerConfiguration$sheets <- "TestSheet_1"
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfiguration = importerConfiguration,
#'   importAllSheets = TRUE
#' )
#' @export
createImporterConfigurationForFile <- function(filePath, sheet = NULL) {
  validateIsString(filePath)

  importerConfiguration <- DataImporterConfiguration$new()
  importerTask <- getNetTask("DataImporterTask")
  if (is.null(sheet)) {
    ref <- rClr::clrCall(importerTask, "CreateConfigurationFor", filePath)
  } else {
    ref <- rClr::clrCall(importerTask, "CreateConfigurationFor", filePath, sheet)
  }
  importerConfiguration$ref <- ref
  # set timeColumn dimension and unit to default ("Time" and "h") if it could not be read from file
  if (importerConfiguration$timeUnit == "?") {
    column <- rClr::clrCall(importerTask, "GetTime", importerConfiguration$ref)
    mappedColumn <- rClr::clrGet(column, "MappedColumn")
    rClr::clrSet(mappedColumn, "Dimension", getDimensionByName(enc2utf8(ospDimensions$Time)))
    importerConfiguration$timeUnit <- ospUnits$Time$h
  }

  return(importerConfiguration)
}
