#' Time-values profile plot for population simulations
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams .extractAggregatedSimulatedData
#'
#' @param quantiles A numerical vector with quantile values (Default: `c(0.05,
#'  0.50, 0.95)`) to be plotted. Ignored if `aggregation` is not `quantiles`.
#'
#' @details
#' For `aggregation = arithmetic`, arithmetic mean with arithmetic standard deviation (SD)
#' will be plotted.
#' For `aggregation = geometric`, geometric mean with geo SD will be plotted
#' For `aggregation = quantiles`, the quantile values defined in the argument `quantiles`
#' will be used. In the profile plot, the middle value will be used to draw
#'  a line, while the lower and upper values will be used as the lower und upper ranges.
#'
#'
#' @import tlf
#'
#' @family plotting
#'
#' @examples
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#'
#' populationResults <- importResultsFromCSV(
#'   simulation = sim,
#'   filePaths = system.file("extdata", "SimResults_pop.csv", package = "ospsuite")
#' )
#'
#' # Create a new instance of `DataCombined` class
#' myDataComb <- DataCombined$new()
#' myDataComb$addSimulationResults(populationResults)
#'
#'
#' # plot
#' plotPopulationTimeProfile(myDataComb)
#'
#' # plot with other quantiles
#' plotPopulationTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))
#'
#' # plot with arithmetic mean
#' plotPopulationTimeProfile(myDataComb,
#'   aggregation = "arithmetic"
#' )
#'
#' @export
plotPopulationTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL,
                                      aggregation = "quantiles",
                                      quantiles = c(0.05, 0.5, 0.95),
                                      ...) {
  .plotTimeProfile(
    dataCombined,
    defaultPlotConfiguration,
    aggregation,
    probs = quantiles,
    ...
  )
}
