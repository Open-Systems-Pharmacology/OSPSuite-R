# Table parameters

Parameters defined by a **table formula** have special syntax for
retrieving and changing the values. The value of a table parameter is
given by a list of `ValuePoint` objects, each `ValuePoint` being an x-y
value pair. X values usually refer to the simulation time.

``` r
library(ospsuite)
#> The option 'ospsuite.plots.watermarkEnabled' is not set.
#> To enable watermarks, add the following to your .Rprofile:
#>   options(ospsuite.plots.watermarkEnabled = TRUE)
#> To disable watermarks, add:
#>   options(ospsuite.plots.watermarkEnabled = FALSE)
#> You can edit your .Rprofile with usethis::edit_r_profile()

# Load a simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|TableParameter" located under container <Vergin 1995 IV>!
print(tableParam)
#> Error:
#> ! object 'tableParam' not found
```

Direct access to the value points is possible through the `TableFormula`
of the table parameter. All x- or y-values stored in the table can be
conveniently retrieved using the `lapply` method:

``` r
# Get the parameter defined by a table
tableParam <- getParameter("Organism|TableParameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|TableParameter" located under container <Vergin 1995 IV>!
print(tableParam)
#> Error:
#> ! object 'tableParam' not found

# Get all value points
tableParam$formula$allPoints
#> Error:
#> ! object 'tableParam' not found

# Get all x-values
xValues <- lapply(tableParam$formula$allPoints, function(point) {
  point$x
})
#> Error:
#> ! object 'tableParam' not found
print(xValues)
#> Error:
#> ! object 'xValues' not found

# Get all y-values
yValues <- lapply(tableParam$formula$allPoints, function(point) {
  point$y
})
#> Error:
#> ! object 'tableParam' not found
print(yValues)
#> Error:
#> ! object 'yValues' not found
```

The method `valueAt()` of the `TableFormula` returns the value of `y`
for the given `x`. If no entry exists for the `x`, the value `y` is
linearly interpolated between the two closest `x` values.

``` r
# Get the parameter defined by a table
tableParam <- getParameter("Organism|TableParameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|TableParameter" located under container <Vergin 1995 IV>!

# Value at x = 60 is stored in the table
tableParam$formula$valueAt(60)
#> Error:
#> ! object 'tableParam' not found

# Value at x = 90 is not in the table
tableParam$formula$valueAt(90)
#> Error:
#> ! object 'tableParam' not found
```

## Changing table parameter values

Simply setting the value of a table-defined parameter using
`setParameterValues` will override the formula and make the parameter
constant.

``` r
# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|TableParameter" located under container <Vergin 1995 IV>!
tableParam
#> Error:
#> ! object 'tableParam' not found

# Set value to a constant. tableParam$isFixedValue is now TRUE
setParameterValues(tableParam, 10)
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found
```

To change the values of the table, a set of methods of the
`TableFormula` is available. The method `addPoints()` adds a set of x-y
values to the existing table. If trying to add a point with the x-value
already present in the table, an error is thrown:

``` r
tableParam <- getParameter("Organism|TableParameter", sim)
#> Error in `.getEntity()`:
#> ! `tryCatch()`: no entity exists for path "Organism|TableParameter" located under container <Vergin 1995 IV>!
tableParam
#> Error:
#> ! object 'tableParam' not found

# Add new points
tableParam$formula$addPoints(c(1, 2, 3), c(5, 6, 7))
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found

# Try to add points with existing x-values
tableParam$formula$addPoints(0, 1)
#> Error:
#> ! object 'tableParam' not found
```

To remove a point from the table, use the method `removePoint()`. It
remove a point if the x value is present in the table and has the
provided y.

``` r
tableParam
#> Error:
#> ! object 'tableParam' not found

# Remove the point (0, 0)
tableParam$formula$removePoint(0, 0)
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found

# Try to remove the point (1, 1). Note that the value for x = 1 is x = 5 in the original table,
# and no point is removed.
tableParam$formula$removePoint(1, 1)
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found

# Try to remove a non-existing point (0, 1). No point is removed.
tableParam$formula$removePoint(1, 1)
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found
```

The `clearPoints()` method removes all points from the table, while
`setPoints()` method is a combination of clearing the table and adding
new points:

``` r
tableParam
#> Error:
#> ! object 'tableParam' not found

tableParam$formula$setPoints(c(1, 2, 3, 4), c(5, 6, 7, 8))
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found

tableParam$formula$clearPoints()
#> Error:
#> ! object 'tableParam' not found
tableParam
#> Error:
#> ! object 'tableParam' not found
```
