.makeDataFrameColumn <- function(dataSet, property) {
  if (length(dataSet[[property]]) == 0) {
    rep(NA, length(dataSet$xValues))
  } else if (length(dataSet[[property]]) == 1) {
    rep(dataSet[[property]], length(dataSet$xValues))
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

  name <- unlist(mapply(makeDataFrameColumn, dataSets, "name"))
  xUnit <- unlist(mapply(makeDataFrameColumn, dataSets, "xUnit"))
  yUnit <- unlist(mapply(makeDataFrameColumn, dataSets, "yUnit"))
  yErrorUnit <- unlist(mapply(makeDataFrameColumn, dataSets, "yErrorUnit"))
  xDimension <- unlist(mapply(makeDataFrameColumn, dataSets, "xDimension"))
  yDimension <- unlist(mapply(makeDataFrameColumn, dataSets, "yDimension"))
  yErrorType <- unlist(mapply(makeDataFrameColumn, dataSets, "yErrorType"))
  yMolWeight <- unlist(mapply(makeDataFrameColumn, dataSets, "molWeight"))
  xValue <- unlist(mapply(makeDataFrameColumn, dataSets, "xValues"))
  yValue <- unlist(mapply(makeDataFrameColumn, dataSets, "yValues"))
  yErrorValues <- unlist(mapply(makeDataFrameColumn, dataSets, "yErrorValues"))

  return(data.frame(
    name, xValue, yValue, yErrorValues, xDimension, xUnit, yDimension,
    yUnit, yErrorType, yErrorUnit, yMolWeight
  ))
}
