% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-entity.R
\name{uniqueEntities}
\alias{uniqueEntities}
\title{Extract Unique Elements of type 'Entity'}
\usage{
uniqueEntities(entities, compareBy = CompareBy$id)
}
\arguments{
\item{entities}{List of objects of type 'Entity'}

\item{compareBy}{A string defining the property that is compared by.
Can take values 'id', 'name', and 'path'. Default is 'id'.}
}
\value{
List of entities that are unique for the property defined by the
argument 'compareBy'.
}
\description{
Extract Unique Elements of type 'Entity'
}
\examples{
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
sim <- loadSimulation(simPath)

parameters <- c(
  getParameter(toPathString(c("Organism", "Liver", "Volume")), sim),
  getParameter(toPathString(c("Organism", "Liver", "Volume")), sim),
  getParameter(toPathString(c("Organism", "TableParameter")), sim)
)

# Return a list containing the two parameters 'Volume' and 'Weight (tissue)'
uniqueEntities(parameters, CompareBy$id)

}
