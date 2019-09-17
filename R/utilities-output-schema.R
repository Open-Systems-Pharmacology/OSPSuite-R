#' Adds an interval to the output schema of the simulation
#'
#' @param simulation Simulation for which a new interval should be created
#' @param startTime Start time of the interval in min
#' @param endTime End time of the interval in min
#' @param resolution Optional resolution in points/min (Default is 4 pts/hr)
#' @param intervalName Optional Name of interval. If not specified, a unique name wil be assigned.
#'
#' @return Returns the interval created.
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)
#'
#' # clears the previous output schema
#' clearIntervals(sim)
#'
#' # Adds a new interval starting at 1h and ending at 10h with a resolution of 10 points per hour
#' addInterval(sim, 1 * 60, 10 * 60, 1 / 6)
#'
#' # Adds another interval starting at 10h and ending at 17h with a
#' # default resolution and a specified name
#' addInterval(sim, 10 * 60, 17 * 60, intervalName = "Second Interval")
#' @export
addInterval <- function(simulation, startTime, endTime, resolution = 4 / 60, intervalName = NULL) {
  validateIsOfType(simulation, "Simulation")
  validateIsNumeric(c(startTime, endTime, resolution))
  schema <- simulation$outputSchema
  outputIntervalFactory <- getNetTask("OutputIntervalFactory")
  interval <- toIntervals(rClr::clrCall(outputIntervalFactory, "CreateFor", schema$ref, startTime, endTime, resolution))
  if (!is.null(intervalName)) {
    interval$name <- intervalName
  }
  schema$addInterval(interval)
  invisible(interval)
}



#' @title  Removes all intervals from the output schema defined in \code{simulation}
#'
#' @param simulation Instance of a simulation for which output intervals should be cleared
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' #Make sure we create a new simulation so that we do not impact other examples
#' sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)
#'
#' clearIntervals(sim)
#' @export
clearIntervals <- function(simulation) {
  validateIsOfType(simulation, "Simulation")
  simulation$outputSchema$clear()
  invisible(simulation)
}


toIntervals <- function(netIntervals) {
  toObjectType(netIntervals, Interval)
}
