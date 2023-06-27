#' Time-values profile plot for population simulations
#'
#' @inheritParams plotIndividualTimeProfile
#' @inheritParams .extractAggregatedSimulatedData
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
#' plotPopulationTimeProfile(myDataComb, probs = c(0.1, 0.5, 0.9))
#'
#' # plot with arithmetic mean
#' plotPopulationTimeProfile(myDataComb,
#'                           aggregation = "arithmetic")
#
#' # plot with arithmetic mean and 3sd above and below average line
#' plotPopulationTimeProfile(myDataComb,
#'                           aggregation = "arithmetic",
#'                           n=3)
#'
#' @export
plotPopulationTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL,
                                      aggregation = "quantiles",
                                      ...) {

  .plotTimeProfile(dataCombined, defaultPlotConfiguration, aggregation, ...)
}
