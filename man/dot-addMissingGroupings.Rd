% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-plotting.R
\name{.addMissingGroupings}
\alias{.addMissingGroupings}
\title{Replace missing groupings with dataset names}
\usage{
.addMissingGroupings(data)
}
\arguments{
\item{data}{A data frame returned by \code{DataCombined$toDataFrame()}.}
}
\description{
Datasets which haven't been assigned to any group will be plotted as a group
on its own. That is, the \code{group} column entries for them will be their names.
}
\examples{

df <- dplyr::tibble(
  group = c(
    "Stevens 2012 solid total",
    "Stevens 2012 solid total",
    NA,
    NA,
    NA
  ),
  name = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Sita_dist",
    "Stevens_2012_placebo.Sita_proximal",
    "Stevens_2012_placebo.Sita_total"
  ),
  dataType = c(
    "simulated",
    "observed",
    "observed",
    "observed",
    "observed"
  )
)

# original
df

# transformed
ospsuite:::.addMissingGroupings(df)

}
\seealso{
Other utilities-plotting: 
\code{\link{.convertGeneralToSpecificPlotConfiguration}()},
\code{\link{.createAxesLabels}()},
\code{\link{.extractAggregatedSimulatedData}()}
}
\concept{utilities-plotting}
\keyword{internal}
