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
#'   importerConfigurationOrPath = importerConfiguration,
#'   importAllSheets = FALSE
#' )
#' @export
createImporterConfigurationForFile <- function(filePath, sheet = NULL) {
  validateIsString(filePath)
  importerTask <- getNetTask("DataImporterTask")
  if (is.null(sheet)) {
    ref <- rClr::clrCall(importerTask, "CreateConfigurationFor", filePath)
  } else {
    ref <- rClr::clrCall(importerTask, "CreateConfigurationFor", filePath, sheet)
  }
  return(DataImporterConfiguration$new(ref))
}

#' Load `DataImporterConfiguration` from XML file.
#'
#' @param configurationFilePath Path to the XML file with stored configuration
#' (e.g. created in PK-Sim or MoBi).
#'
#' @return A new `DataImporterConfiguration` object to be used with
#' `loadDataSetsFromExcel()`.
#' @export
#'
#' @examples
#'
#' configurationFilePath <- system.file(
#'   "extdata", "dataImporterConfiguration.xml",
#'   package = "ospsuite"
#' )
#'
#' importerConfiguration <- loadDataImporterConfiguration(configurationFilePath)
#'
#' # Specifying which sheet to load
#' importerConfiguration$sheets <- "TestSheet_1"
#' xlsFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfigurationOrPath = importerConfiguration,
#'   importAllSheets = FALSE
#' )
loadDataImporterConfiguration <- function(configurationFilePath) {
  validateIsString(configurationFilePath)
  importerTask <- getNetTask("DataImporterTask")
  ref <- rClr::clrCall(importerTask, "GetConfiguration", configurationFilePath)
  return(DataImporterConfiguration$new(ref))
}
