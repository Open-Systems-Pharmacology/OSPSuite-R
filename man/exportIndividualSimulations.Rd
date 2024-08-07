% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-simulation.R
\name{exportIndividualSimulations}
\alias{exportIndividualSimulations}
\title{Export simulation PKMLs for given \code{individualIds}. Each pkml file will contain the original simulation updated with parameters of the corresponding individual.}
\usage{
exportIndividualSimulations(
  population,
  individualIds,
  outputFolder,
  simulation
)
}
\arguments{
\item{population}{A population object typically loaded with \code{loadPopulation}}

\item{individualIds}{Ids of individual (single value or array) to export}

\item{outputFolder}{Folder where the individual simulations will be exported. File format will be \code{simulationName_individualId}}

\item{simulation}{Simulation uses to generate PKML files}
}
\value{
An array containing the path of all exported simulations.
}
\description{
Export simulation PKMLs for given \code{individualIds}. Each pkml file will contain the original simulation updated with parameters of the corresponding individual.
}
\examples{
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

popPath <- system.file("extdata", "simple_pop.csv", package = "ospsuite")
population <- loadPopulation(popPath)

exportIndividualSimulations(population, c(1, 2), tempdir(), sim)
}
