% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-output-selections.R
\name{clearOutputs}
\alias{clearOutputs}
\title{Removes all selected output from the given \code{simulation}}
\usage{
clearOutputs(simulation)
}
\arguments{
\item{simulation}{Instance of a simulation for which output selection should be cleared.}
}
\description{
Removes all selected output from the given \code{simulation}
}
\examples{

simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath, )

clearOutputs(sim)
}
