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
#' # When sheet is specified, it is automatically added to the configuration
#' importerConfiguration <- createImporterConfigurationForFile(
#'   xlsFilePath,
#'   sheet = "TestSheet_1"
#' )
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfigurationOrPath = importerConfiguration,
#'   importAllSheets = FALSE
#' )
#' @export
createImporterConfigurationForFile <- function(filePath, sheet = NULL) {
  validateIsString(filePath)
  importerTask <- .getNetTaskFromCache("DataImporterTask")
  if (is.null(sheet)) {
    ref <- importerTask$call("CreateConfigurationFor", filePath)
  } else {
    ref <- importerTask$call("CreateConfigurationFor", filePath, sheet)
  }
  configuration <- DataImporterConfiguration$new(ref)
  
  # Set the sheets attribute if a sheet was specified.
  # This populates the initially empty sheets list with the sheet
  # used to create the configuration from the .NET backend.
  if (!is.null(sheet)) {
    configuration$sheets <- sheet
  }
  
  return(configuration)
}

#' Load `DataImporterConfiguration` from XML file.
#'
#' @param configurationFilePath Path to the XML file with stored configuration
#' (e.g. created in PK-Sim or MoBi).
#'
#' @return A new `DataImporterConfiguration` object to be used with
#' `loadDataSetsFromExcel()`.
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
#'
#' @export
loadDataImporterConfiguration <- function(configurationFilePath) {
  validateIsString(configurationFilePath)
  importerTask <- .getNetTaskFromCache("DataImporterTask")
  ref <- importerTask$call("GetConfiguration", configurationFilePath)
  return(DataImporterConfiguration$new(ref))
}
