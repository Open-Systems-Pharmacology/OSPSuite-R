% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-population.R
\name{createPopulation}
\alias{createPopulation}
\title{Creates an population using the PK-Sim Database}
\usage{
createPopulation(populationCharacteristics)
}
\arguments{
\item{populationCharacteristics}{Characteristics of the population to create as an instance of \code{OriginData}
that are actually distributed parameters}
}
\value{
An list with three entries:
\itemize{
\item \code{population} An instance of a population object.
\item \code{derivedParameters} containing the parameter values modified indirectly by the algorithm. Those parameters are typically formula parameters.
\item \code{seed} containing the seed value used to generate random values
}
}
\description{
Creates an population using the PK-Sim Database
}
