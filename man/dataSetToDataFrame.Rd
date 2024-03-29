% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-data-set.R
\name{dataSetToDataFrame}
\alias{dataSetToDataFrame}
\alias{dataSetToTibble}
\title{Converts a list of \code{DataSet} objects to a data.frame}
\usage{
dataSetToDataFrame(dataSets)

dataSetToTibble(dataSets)
}
\arguments{
\item{dataSets}{A list of \code{DataSet} objects or a single \code{DataSet}}
}
\value{
DataSet objects as data.frame with columns name, xValues, yValues,
yErrorValues, xDimension, xUnit, yDimension, yUnit, yErrorType, yErrorUnit,
molWeight, lloq, and a column for each meta data that is present in any
\code{DataSet}.
}
\description{
Converts a list of \code{DataSet} objects to a data.frame
}
