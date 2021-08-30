.makeDataFrameColumn <- function(dataSet, property, metaDataName = NULL) {
  # check length of entry for a certain property of this data set, i.e. if it exists
  if (is.null(metaDataName)) {
    len <- length(dataSet[[property]])
  } else {
    len <- length(dataSet[[property]][[metaDataName]])
  }

  if (len == 0) {
    rep(NA, length(dataSet$xValues))
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

#' Loads data (typically observed data) from a PKML file and creates a \code{DataSet} from it.
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
  return(DataSet$new(dataRepository))
}

#' Save the \code{DataSet} to pkml
#' @details Save the \code{DataSet} to a pkml file that can be loaded by MoBi
#'
#' @param dataSet The \code{DataSet} object
#' @param filePath Path where the pkml file will be created
#' @export
#'
#' @examples
#' \dontrun{
#' dataSet <- DataSet$new()
#' dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
#' dataSet$saveToPKML(filePath = "../ObsData.pkml")
#' }
saveDataSetToPKML <- function(dataSet, filePath) {
  validateIsString(filePath)
  validateIsOfType(dataSet, DataSet)
  filePath <- expandPath(filePath)
  dataRepositoryTask <- getNetTask("DataRepositoryTask")
  rClr::clrCall(dataRepositoryTask, "SaveDataRepository", dataSet$dataRepository$ref, filePath)
}

#' Converts a list of DataSet objects to a data.frame
#'
#' @param dataSets A list of DataSet objects or a single DataSet
#'
#' @return DataSet objects as data.frame with columns name, xValue, yValue, yErrorValues,
#' xDimension, xUnit, yDimension, yUnit, yErrorType, yErrorUnit, yMolWeight
#' @export
dataSetToDataFrame <- function(dataSets) {
  dataSets <- c(dataSets)
  validateIsOfType(dataSets, DataSet)

  name <- unlist(mapply(.makeDataFrameColumn, dataSets, "name"))
  xUnit <- unlist(mapply(.makeDataFrameColumn, dataSets, "xUnit"))
  yUnit <- unlist(mapply(.makeDataFrameColumn, dataSets, "yUnit"))
  yErrorUnit <- as.character(unlist(mapply(.makeDataFrameColumn, dataSets, "yErrorUnit")))
  xDimension <- unlist(mapply(.makeDataFrameColumn, dataSets, "xDimension"))
  yDimension <- unlist(mapply(.makeDataFrameColumn, dataSets, "yDimension"))
  yErrorType <- as.character(unlist(mapply(.makeDataFrameColumn, dataSets, "yErrorType")))
  yMolWeight <- unlist(mapply(.makeDataFrameColumn, dataSets, "molWeight"))
  xValue <- unlist(mapply(.makeDataFrameColumn, dataSets, "xValues"))
  yValue <- unlist(mapply(.makeDataFrameColumn, dataSets, "yValues"))
  yErrorValues <- unlist(mapply(.makeDataFrameColumn, dataSets, "yErrorValues"))

  df <- data.frame(
    name, xValue, yValue, yErrorValues, xDimension, xUnit, yDimension,
    yUnit, yErrorType, yErrorUnit, yMolWeight
  )

  # get all names of meta data entries from all data sets
  metaDataNames <- unique(unlist(sapply(dataSets, function(dataSets) {
    return(names(dataSets[["metaData"]]))
  })))
  # add one column for each one
  for (name in metaDataNames) {
    col <- unlist(lapply(dataSets, .makeDataFrameColumn, "metaData", metaDataName = name))
    df[[name]] <- col
  }

  return(df)
}


loadDataSetsFromXls <- function(xlsFilePath, importerConfiguration, importAllSheets = FALSE) {
  validateIsString(xlsFilePath)
  validateIsOfType(importerConfiguration, DataImporterConfiguration)
  validateIsLogical(importAllSheets)

  dataImporterTask <- getNetTask("DataImporterTask")
  rClr::clrSet(dataImporterTask, "IgnoreSheetNamesAtImport", importAllSheets)
  dataRepositories <- rClr::clrCall(dataImporterTask, "ImportExcelFromConfiguration", importerConfiguration$ref, xlsFilePath)
  dataSets <- lapply(dataRepositories, \(x){
    repository <- DataRepository$new(x)
    DataSet$new(repository)
  })
  names(dataSets) <- lapply(dataSets, \(x){
    x$name
  })

  return(dataSets)
}
