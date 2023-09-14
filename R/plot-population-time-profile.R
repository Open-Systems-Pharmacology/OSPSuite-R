#' Time-values profile plot for population simulations
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams .extractAggregatedSimulatedData

#' @details
#' For `aggregation = quantiles` (default), the quantile values defined in the
#' argument `quantiles` will be used. In the profile plot, the middle value will
#' be used to draw a line, while the lower and upper values will be used as the
#' lower und upper ranges.
#' For `aggregation = arithmetic`, arithmetic mean with arithmetic standard
#' deviation (SD) will be plotted. Use the optional parameter `n` to change the
#' number of SD to plot above and below the mean.
#' For `aggregation = geometric`, geometric mean with geometric standard
#' deviation (SD) will be plotted. Use the optional parameter `n` to change the
#' number of SD to plot above and below the mean.
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
