% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-units.R
\name{getDimensionByName}
\alias{getDimensionByName}
\title{Get dimension by name}
\usage{
getDimensionByName(name)
}
\arguments{
\item{name}{Name of dimension that should be retrieved}
}
\value{
Returns the an instance of the dimension with the given name if found or \code{NULL}
otherwise.
}
\description{
Get dimension by name
}
\examples{

getDimensionByName("Time")
}
