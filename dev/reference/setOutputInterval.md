# Clears the output interval from the simulation and adds a new one.

Clears the output interval from the simulation and adds a new one.

## Usage

``` r
setOutputInterval(
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

## Note

This is essentially a shortcut for `clearOutputIntervals` followed by
`addOutputInterval`

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Load the simulation
sim <- loadSimulation(simPath, addToCache = FALSE, loadFromCache = FALSE)

# Adds a new interval starting at 1h and ending at 10h with a resolution of 10 points per hour
setOutputInterval(sim, 1 * 60, 10 * 60, 1 / 6)
```
