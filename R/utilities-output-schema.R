#' Adds an interval to the output schema of the simulation
#'
#' @param simulation Simulation for which a new interval should be created
#' @param startTime Start time of the interval in min
#' @param endTime End time of the interval in min
#' @param resolution resolution in points/min
#' @param intervalName Optional Name of interval. If not specified, a unique name will be assigned.
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
#' clearOutputIntervals(sim)
#'
#' # Adds a new interval starting at 1h and ending at 10h with a resolution of 10 points per hour
#' addOutputInterval(sim, 1 * 60, 10 * 60, 1 / 6)
#'
#' # Adds another interval starting at 10h and ending at 17h with a resolution of 4 points per hour
#' # and a specified name
#' addOutputInterval(sim, 10 * 60, 17 * 60, 4 / 60, intervalName = "Second Interval")
#' @export
addOutputInterval <- function(simulation, startTime, endTime, resolution, intervalName = NULL) {
  validateIsOfType(simulation, Simulation)
  validateIsNumeric(c(startTime, endTime, resolution))
  schema <- simulation$outputSchema
  outputIntervalFactory <- getNetTask("OutputIntervalFactory")
  netIntervals <- rClr::clrCall(outputIntervalFactory, "CreateFor", schema$ref, startTime, endTime, resolution)
  interval <- toObjectType(netIntervals, Interval)
  if (!is.null(intervalName)) {
    interval$name <- intervalName
  }
  schema$addInterval(interval)
  invisible(interval)
}

#' Clears the output interval from the simulation and adds a new one.
#'
#' @inherit addOutputInterval
#'
#' @note This is essentially a shortcut for `clearOutputIntervals` followed by `addOutputInterval`
#'
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Load the simulation
#' sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)
#'
#' # Adds a new interval starting at 1h and ending at 10h with a resolution of 10 points per hour
#' setOutputInterval(sim, 1 * 60, 10 * 60, 1 / 6)
#' @export
setOutputInterval <- function(simulation, startTime, endTime, resolution, intervalName = NULL) {
  clearOutputIntervals(simulation)
  addOutputInterval(simulation, startTime, endTime, resolution, intervalName)
}

#' @title  Removes all intervals as well as all single time points from the output schema
#' defined in `simulation`
#'
#' @param simulation Instance of a simulation for which output intervals should be cleared
#'
#' @examples
#'
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#'
#' # Make sure we create a new simulation so that we do not impact other examples
#' sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)
#'
#' clearOutputIntervals(sim)
#' @export
clearOutputIntervals <- function(simulation) {
  validateIsOfType(simulation, Simulation)
  simulation$outputSchema$clear()
  invisible(simulation)
}
