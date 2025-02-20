% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-quantity.R
\name{.getParentContainerByType}
\alias{.getParentContainerByType}
\title{Get the parent container of a specific type for a given Entity}
\usage{
.getParentContainerByType(entity, type)
}
\arguments{
\item{entity}{An \code{Entity} object or an object inheriting from \code{Entity}}

\item{type}{A string representing the container type to find (e.g.,
"Simulation", "Molecule").}
}
\value{
The closest parent container of the specified type or \code{NA} if not found.
}
\description{
Recursively retrieves the parent container of the specified type
for a given \code{Entity}.
}
\keyword{internal}
