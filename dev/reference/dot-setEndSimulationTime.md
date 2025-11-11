# Set end time of the simulation

Either extends or shortens the simulation time to the specified end
time. Time points that are later than the specified end time are
removed. Intervals that start after the specified end time are removed.
Intervals that start before the specified end time are shortened to the
specified end time.

## Usage

``` r
.setEndSimulationTime(simulation, endTime)
```

## Arguments

- simulation:

  Simulation for which the end time should be set

- endTime:

  End time of the simulation in min
