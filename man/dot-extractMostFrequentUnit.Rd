% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-units.R
\name{.extractMostFrequentUnit}
\alias{.extractMostFrequentUnit}
\title{Find the most common units}
\usage{
.extractMostFrequentUnit(data, unitColumn)
}
\arguments{
\item{data}{A data frame (or a tibble) from \code{DataCombined$toDataFrame()}.}

\item{unitColumn}{The name of the column containing units (e.g. \code{xUnit}).}
}
\description{
Find the most common units
}
\examples{

df <- dplyr::tibble(
  xValues = c(15, 30, 60),
  xUnit = "min",
  xDimension = "Time",
  yValues = c(0.25, 45, 78),
  yUnit = c("", "\%", "\%"),
  yErrorUnit = c("", "\%", "\%"),
  yDimension = "Fraction",
  molWeight = 10
)

ospsuite:::.extractMostFrequentUnit(df, unitColumn = "xUnit")
ospsuite:::.extractMostFrequentUnit(df, unitColumn = "yUnit")

}
\keyword{internal}
