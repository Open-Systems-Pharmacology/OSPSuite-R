# Load a simulation from a pkml file

Loads a simulation from a pkml file and returns the simulation. If the
passed simulation file has been loaded before, the simulation is not
loaded again but a cached object is returned. This behavior can be
overridden.

## Usage

``` r
loadSimulation(
  filePath,
  loadFromCache = FALSE,
  addToCache = TRUE,
  resetIds = TRUE
)
```

## Arguments

- filePath:

  Full path of pkml simulation file to load.

- loadFromCache:

  If `TRUE`, an already loaded pkml file will not be loaded again, but
  the simulation object will be retrieved from cache. If `FALSE`, a new
  simulation object will be created. Default value is `FALSE`.

- addToCache:

  If `TRUE`, the loaded simulation is added to cache. If `FALSE`, the
  returned simulation only exists locally. Default is `TRUE`.

- resetIds:

  If `TRUE`, the internal object ids in the simulation are reset to a
  unique value. If `FALSE`, the ids are kept as defined in the pkml
  simulation. Default is `TRUE`.

## Examples

``` r
simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")

# Load sim1 for the first time
sim1 <- loadSimulation(simPath)

# sim2 will be loaded from cache and will represent the same object as sim1
sim2 <- loadSimulation(simPath, loadFromCache = TRUE)

parameter1 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim1)
parameter2 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim2)

# parameter1 and parameter2 belong to the same simulation object, so changing
# one of the them will also change another
setParameterValues(parameters = parameter2, values = 0)
parameter1$value == parameter2$value # TRUE
#> [1] TRUE

# sim3 will not be loaded from cache
sim3 <- loadSimulation(simPath, loadFromCache = FALSE)
# parameter3 belong to different simulation object than parameter1 and parameter2
parameter3 <- getParameter(toPathString(c("Organism", "Liver", "Volume")), sim3)
setParameterValues(parameters = parameter3, values = 1)
parameter2$value == parameter3$value # FALSE#'
#> [1] FALSE
```
