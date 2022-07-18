#' Time-values profile plot for population simulations
#'
#' @inheritParams plotIndividualTimeProfile
#' @param quantiles A numerical vector with quantile values (Default: `c(0.05,
#'   0.50, 0.95)`), with the quantile values defining the aggregation of
#'   individual data. In the profile plot, the middle value will be used to draw
#'   a line, while the lower and upper values will be used to create a ribbon.
#'
#' @import tlf
#'
#' @family plotting
#'
#' @examples
#'
#' # TODO: add example
#'
#' @export
plotPopulationTimeProfile <- function(dataCombined,
                                      defaultPlotConfiguration = NULL,
                                      quantiles = c(0.05, 0.5, 0.95)) {
  validateIsNumeric(quantiles, nullAllowed = FALSE)
  validateIsOfLength(quantiles, 3L)

  .plotTimeProfile(dataCombined, defaultPlotConfiguration, quantiles)
}
