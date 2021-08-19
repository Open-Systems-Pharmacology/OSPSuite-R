.makeDataFrameColumn <- function(dataSet, property, metaDataName = NULL) {
  # check length of entry for a certain property of this data set, i.e. if it exists
  if (is.null(metaDataName)) {
    len <- length(dataSet[[property]])
  } else {
    # len <- min(length(dataSet[[property]][[metaDataName]]), 1)
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
