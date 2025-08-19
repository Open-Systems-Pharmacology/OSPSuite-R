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
  validateIsOfType(simulation, "Simulation")
  validateIsNumeric(c(startTime, endTime, resolution))
  schema <- simulation$outputSchema
  outputIntervalFactory <- .getCoreTask("OutputIntervalFactory")
  netIntervals <- outputIntervalFactory$call("CreateFor", schema, startTime, endTime, resolution)
  interval <- .toObjectType(netIntervals, Interval)
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
  validateIsOfType(simulation, "Simulation")
  simulation$outputSchema$clear()
  invisible(simulation)
}

#' Set end time of the simulation
#' @description
#' Either extends or shortens the simulation time to the specified end time.
#' Time points that are later than the specified end time are removed.
#' Intervals that start after the specified end time are removed.
#' Intervals that start before the specified end time are shortened to the specified end time.
#'
#' @param simulation Simulation for which the end time should be set
#' @param endTime End time of the simulation in min
.setEndSimulationTime <- function(simulation, endTime) {
  ospsuite.utils::validateIsOfType(simulation, "Simulation")
  ospsuite.utils::validateIsNumeric(endTime)
  if (endTime <= 0) {
    stop(messages$valueNotPositive(endTime, "endTime"))
  }

  # If the specified end time is outside of the current end time,
  # extend the simulation time to the specified end time.
  if (endTime > simulation$outputSchema$endTime) {
    simulation$outputSchema$addTimePoints(timePoints = endTime)
  } else {
    # Otherwise, shorten the simulation time to the specified end time.
    # First remove the time points that are later than the specified steady-state time.
    timePoints <- simulation$outputSchema$timePoints
    timePoints <- timePoints[timePoints <= endTime]

    # Get the old output intervals
    oldOutputIntervals <- simulation$outputSchema$intervals

    # Clear the output schema
    simulation$outputSchema$clear()
    # Iterate through output intervals and only add those that start before the
    # specified steady-state time.
    for (outputInterval in oldOutputIntervals) {
      if (outputInterval$startTime$value <= endTime) {
        addOutputInterval(
          simulation = simulation,
          startTime = outputInterval$startTime$value,
          endTime = min(outputInterval$endTime$value, endTime),
          resolution = outputInterval$resolution$value,
          intervalName = outputInterval$name
        )
      }
    }

    # Don't forget to add latest time
    simulation$outputSchema$addTimePoints(c(timePoints, endTime))
  }
}
