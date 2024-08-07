% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-population.R
\name{populationToDataFrame}
\alias{populationToDataFrame}
\alias{populationToTibble}
\title{Creates a data.frame containing one column for each parameter defined in the population}
\usage{
populationToDataFrame(population)

populationToTibble(population)
}
\arguments{
\item{population}{Population to convert to data frame (typically imported from file using \code{loadPopulation})}
}
\description{
Creates a data.frame containing one column for each parameter defined in the population
}
\examples{
csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")

population <- loadPopulation(csvPath)
df <- populationToDataFrame(population)
}
