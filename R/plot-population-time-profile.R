#' Time-values profile plot for population simulations
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams .extractAggregatedSimulatedData
#' @param ... additionnal arguments to pass to `.extractAggregatedSimulatedData()`
#'
#' @inherit .extractAggregatedSimulatedData details
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
