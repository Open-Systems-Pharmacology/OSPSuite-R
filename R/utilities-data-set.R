
makeColumn <- function(dataSet, property) {
  if (length(dataSet[[property]]) == 0) {
    rep(NA, length(dataSet$xValues))
  } else if (length(dataSet[[property]]) == 1) {
    rep(dataSet[[property]], length(dataSet$xValues))
  } else {
    dataSet[[property]]
  }
}

# # retrieve entry 'property' from a DataSet object
# makeXYColumn <- function(dataSet, property) {
#   if (length(dataSet[[property]]) == 0) {
#     rep(NA, length(dataSet$xValues))
#   } else {
#     dataSet[[property]]
#   }
# }

#' Converts a list of DataSet objects to a data.frame
#'
#' @param dataSets A list of DataSet objects or a single DataSet
#'
#' @return DataSet objects as dataframe with columns name, xValue, yValue, yErrorValues,
#' xDimension, xUnit, yDimension, yUnit, yErrorType, yErrorUnit, yMolWeight
#' @export
dataSetToDataFrame <- function(dataSets) {
  dataSets <- c(dataSets)
  validateIsOfType(dataSets, DataSet)

  name <- unlist(mapply(makeColumn, dataSets, "name"))
  xUnit <- unlist(mapply(makeColumn, dataSets, "xUnit"))
  yUnit <- unlist(mapply(makeColumn, dataSets, "yUnit"))
  yErrorUnit <- unlist(mapply(makeColumn, dataSets, "yErrorUnit"))
  xDimension <- unlist(mapply(makeColumn, dataSets, "xDimension"))
  yDimension <- unlist(mapply(makeColumn, dataSets, "yDimension"))
  yErrorType <- unlist(mapply(makeColumn, dataSets, "yErrorType"))
  yMolWeight <- unlist(mapply(makeColumn, dataSets, "molWeight"))
  xValue <- unlist(mapply(makeColumn, dataSets, "xValues"))
  yValue <- unlist(mapply(makeColumn, dataSets, "yValues"))
  yErrorValues <- unlist(mapply(makeColumn, dataSets, "yErrorValues"))

  return(data.frame(name, xValue, yValue, yErrorValues, xDimension, xUnit, yDimension,
                    yUnit, yErrorType, yErrorUnit, yMolWeight))
}
