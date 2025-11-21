# Adds an interval to the output schema of the simulation

Adds an interval to the output schema of the simulation

## Usage

``` r
addOutputInterval(
  simulation,
  startTime,
  endTime,
  resolution,
  intervalName = NULL
)
```

## Arguments

- simulation:

  Simulation for which a new interval should be created

- startTime:

  Start time of the interval in min

- endTime:

  End time of the interval in min

- resolution:

  resolution in points/min

- intervalName:

  Optional Name of interval. If not specified, a unique name will be
  assigned.

## Value

Returns the interval created.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)

# clears the previous output schema
clearOutputIntervals(sim)

# Adds a new interval starting at 1h and ending at 10h with a resolution of 10 points per hour
addOutputInterval(sim, 1 * 60, 10 * 60, 1 / 6)

# Adds another interval starting at 10h and ending at 17h with a resolution of 4 points per hour
# and a specified name
addOutputInterval(sim, 10 * 60, 17 * 60, 4 / 60, intervalName = "Second Interval")
```
