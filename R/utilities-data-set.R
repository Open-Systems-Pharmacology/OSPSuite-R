.makeDataFrameColumn <- function(dataSets, property, metaDataName = NULL) {
  columForDataSet <- function(dataSet) {
    # check length of entry for a certain property of this data set, i.e. if it exists
    if (is.null(metaDataName)) {
      len <- length(dataSet[[property]])
    } else {
      len <- length(dataSet[[property]][[metaDataName]])
    }

    if (len == 0) {
      rep(NA_real_, length(dataSet$xValues))
    } else if (len == 1) {
      if (is.null(metaDataName)) {
        rep(dataSet[[property]], length(dataSet$xValues))
      } else {
        rep(dataSet[[property]][[metaDataName]], length(dataSet$xValues))
      }
    } else {
      dataSet[[property]]
    }
  }

  unlist(lapply(dataSets, function(x) {
    columForDataSet(x)
  }), use.names = FALSE)
}

#' Loads data (typically observed data) from a PKML file and creates a `DataSet` from it.
#' The pkml files are typically exported from PK-Sim or MoBi.
#'
#' @param filePath Full path of pkml file containing the observed data to load
#'
#' @examples
#' filePath <- system.file("extdata", "obs_data.pkml", package = "ospsuite")
#'
#' obsData <- loadDataSetFromPKML(filePath)
#' @export
loadDataSetFromPKML <- function(filePath) {
  dataRepository <- .loadDataRepositoryFromPKML(filePath)
  return(DataSet$new(dataRepository = dataRepository))
}

#' Save the `DataSet` to pkml
#' @details Save the `DataSet` to a pkml file that can be loaded by MoBi
#'
#' @param dataSet The `DataSet` object
#' @param filePath Path where the pkml file will be created
#' @export
#'
#' @examples
#' \dontrun{
#' dataSet <- DataSet$new(name = "NewDataSet")
#' dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
#' dataSet$saveToPKML(filePath = "../ObsData.pkml")
#' }
saveDataSetToPKML <- function(dataSet, filePath) {
  validateIsString(filePath)
  validateIsOfType(dataSet, "DataSet")
  filePath <- expandPath(filePath)
  dataRepositoryTask <- .getNetTask("DataRepositoryTask")
  rClr::clrCall(dataRepositoryTask, "SaveDataRepository", dataSet$dataRepository$ref, filePath)
}

#' Converts a list of `DataSet` objects to a data.frame
#'
#' @param dataSets A list of `DataSet` objects or a single `DataSet`
#'
#' @return
#'
#' DataSet objects as data.frame with columns name, xValues, yValues,
#' yErrorValues, xDimension, xUnit, yDimension, yUnit, yErrorType, yErrorUnit,
#' molWeight, lloq, and a column for each meta data that is present in any
#' `DataSet`.
#'
#' @export
dataSetToDataFrame <- function(dataSets) {
  dataSets <- c(dataSets)
  validateIsOfType(dataSets, "DataSet")

  name <- .makeDataFrameColumn(dataSets, "name")
  xUnit <- .makeDataFrameColumn(dataSets, "xUnit")
  yUnit <- .makeDataFrameColumn(dataSets, "yUnit")
  yErrorUnit <- .makeDataFrameColumn(dataSets, "yErrorUnit")
  xDimension <- .makeDataFrameColumn(dataSets, "xDimension")
  yDimension <- .makeDataFrameColumn(dataSets, "yDimension")
  yErrorType <- .makeDataFrameColumn(dataSets, "yErrorType")
  molWeight <- .makeDataFrameColumn(dataSets, "molWeight")
  xValues <- .makeDataFrameColumn(dataSets, "xValues")
  yValues <- .makeDataFrameColumn(dataSets, "yValues")
  yErrorValues <- .makeDataFrameColumn(dataSets, "yErrorValues")
  lloq <- .makeDataFrameColumn(dataSets, "LLOQ")

  obsData <- data.frame(
    name,
    xValues,
    yValues,
    yErrorValues,
    xDimension,
    xUnit,
    yDimension,
    yUnit,
    yErrorType,
    yErrorUnit,
    molWeight,
    lloq,
    stringsAsFactors = FALSE
  )

  # get all names of meta data entries from all data sets
  metaDataNames <- unique(unlist(lapply(dataSets, function(dataSets) {
    names(dataSets[["metaData"]])
  }), use.names = FALSE))

  # add one column for each one
  for (name in metaDataNames) {
    col <- .makeDataFrameColumn(dataSets, "metaData", metaDataName = name)
    obsData[[name]] <- col
  }

  # consistently return a (classical) data frame
  return(obsData)
}

#' @rdname dataSetToDataFrame
#'
#' @export
dataSetToTibble <- function(dataSets) {
  obsData <- dataSetToDataFrame(dataSets)

  # consistently return a tibble data frame
  return(dplyr::as_tibble(obsData))
}

#' Load data sets from excel
#'
#' @details Load observed data from an excel file using an importer configuration
#'
#' @param xlsFilePath Path to the excel file with the data
#' @param importerConfigurationOrPath An object of type `DataImporterConfiguration` that is valid
#'  for the excel file or a path to a XML file with stored configuration
#' @param importAllSheets If `FALSE` (default), only sheets specified in the
#' `importerConfiguration` will be loaded. If `TRUE`, an attempt to load all sheets
#' is performed. If any sheet does not comply with the configuration, an error is thrown.
#'
#' @return A named set of `DataSet` objects. The naming is defined by the property
#' `importerConfiguration$namingPattern`.
#' @export
#'
#' @examples
#'
#' xlsFilePath <- system.file(
#'   "extdata", "CompiledDataSet.xlsx",
#'   package = "ospsuite"
#' )
#'
#' importerConfiguration <- createImporterConfigurationForFile(xlsFilePath)
#' importerConfiguration$sheets <- "TestSheet_1"
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfigurationOrPath = importerConfiguration,
#'   importAllSheets = FALSE
#' )
#'
#' importerConfigurationFilePath <- system.file(
#'   "extdata", "dataImporterConfiguration.xml",
#'   package = "ospsuite"
#' )
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfigurationOrPath = importerConfigurationFilePath,
#'   importAllSheets = FALSE
#' )
loadDataSetsFromExcel <- function(xlsFilePath, importerConfigurationOrPath, importAllSheets = FALSE) {
  validateIsString(xlsFilePath)
  importerConfiguration <- importerConfigurationOrPath
  if (is.character(importerConfigurationOrPath)) {
    importerConfiguration <- loadDataImporterConfiguration(importerConfigurationOrPath)
  }
  validateIsOfType(importerConfiguration, "DataImporterConfiguration")
  validateIsLogical(importAllSheets)

  dataImporterTask <- .getNetTask("DataImporterTask")
  rClr::clrSet(dataImporterTask, "IgnoreSheetNamesAtImport", importAllSheets)
  dataRepositories <- rClr::clrCall(dataImporterTask, "ImportExcelFromConfiguration", importerConfiguration$ref, xlsFilePath)
  dataSets <- lapply(dataRepositories, function(x) {
    repository <- DataRepository$new(x)
    DataSet$new(dataRepository = repository)
  })
  names(dataSets) <- lapply(dataSets, function(x) {
    x$name
  })

  return(dataSets)
}
