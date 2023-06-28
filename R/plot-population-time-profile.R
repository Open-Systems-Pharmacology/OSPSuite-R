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
#' # Previous quantiles argument still works
#' plotPopulationTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))
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

  # Make previous "quantiles" argument deprecated but still working
  lifecycle::deprecate_warn(when = "11.2.0",
                            what = "plotPopulationTimeProfile(quantiles)",
                            with = "plotPopulationTimeProfile(...)",
                            details = "Use ... to pass extra parameters to
                            aggregating functions.
                            `probs` for `stats::quantile` or `n` for the number
                            of standard deviation to add below and above the
                            average for `arithmetic` or `geometric`."
  )

  args <- list(...)
  # set probs argument
  if ("quantiles" %in% names(args)) {
    args$probs <- args$quantiles
  }

  do.call(.plotTimeProfile,
          args = list(dataCombined = dataCombined,
                      defaultPlotConfiguration = defaultPlotConfiguration,
                      aggregation = aggregation,
                      ... = args))
}
