% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-data-set.R
\name{.makeDataFrameColumn}
\alias{.makeDataFrameColumn}
\title{Create a vector of the right length for a certain property of a \code{DataSet}}
\usage{
.makeDataFrameColumn(dataSets, property, metaDataName = NULL)
}
\arguments{
\item{dataSets}{A list of \code{dataSet} objects or a single \code{dataSet}.}

\item{property}{The property to create the vector for.}

\item{metaDataName}{The name of the metaData to create the vector for.}
}
\value{
A vector of length corresponding to dataSet$xValues containing the property values.
}
\description{
Create a vector of the right length for a certain property of a \code{DataSet}
}
\keyword{internal}
