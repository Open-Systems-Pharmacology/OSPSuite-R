% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-units.R
\name{getDimensionForUnit}
\alias{getDimensionForUnit}
\title{Get dimension for a given unit}
\usage{
getDimensionForUnit(unit)
}
\arguments{
\item{unit}{Unit used to find the corresponding dimension.}
}
\value{
Returns the name of dimension that can be used to support the given unit or
\code{NULL} if the dimension cannot be found.
}
\description{
Get dimension for a given unit
}
\examples{

getDimensionForUnit("mg")
}
