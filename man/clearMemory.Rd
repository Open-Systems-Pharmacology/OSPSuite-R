% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{clearMemory}
\alias{clearMemory}
\title{Clears the memory used by all underlying objects}
\usage{
clearMemory(clearSimulationsCache = FALSE)
}
\arguments{
\item{clearSimulationsCache}{optional - Should the simulation cache also be
cleared? Default is \code{FALSE}.}
}
\description{
Clears the memory used by all underlying objects
}
\details{
The function aims at clearing the memory used by object references allocated
during some workflows. The memory should typically be freed automatically
when the system is under memory pressure or when the garbage collection is
kicking in. However, it may be necessary sometimes to explicitly start the
garbage collection process.
}
\examples{

# This will clear the memory and also clear the simulations cache but leave
# the environment intact.
clearMemory(clearSimulationsCache = TRUE)

}
