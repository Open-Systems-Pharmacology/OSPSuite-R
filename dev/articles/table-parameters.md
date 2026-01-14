# Table parameters

Parameters defined by a **table formula** have special syntax for
retrieving and changing the values. The value of a table parameter is
given by a list of `ValuePoint` objects, each `ValuePoint` being an x-y
value pair. X values usually refer to the simulation time.

``` r
library(ospsuite)

# Load a simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
print(tableParam)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 0.00e+00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: FALSE
```

Direct access to the value points is possible through the `TableFormula`
of the table parameter. All x- or y-values stored in the table can be
conveniently retrieved using the `lapply` method:

``` r
# Get the parameter defined by a table
tableParam <- getParameter("Organism|TableParameter", sim)
print(tableParam)
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 0.00e+00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: FALSE

# Get all value points
tableParam$formula$allPoints
#> [[1]]
#>   x= 0, y= 0, restartSolver= FALSE
#> 
#> [[2]]
#>   x= 60, y= 1, restartSolver= FALSE
#> 
#> [[3]]
#>   x= 120, y= 2, restartSolver= FALSE
#> 
#> [[4]]
#>   x= 180, y= 3, restartSolver= FALSE

# Get all x-values
xValues <- lapply(tableParam$formula$allPoints, function(point) {
  point$x
})
print(xValues)
#> [[1]]
#> [1] 0
#> 
#> [[2]]
#> [1] 60
#> 
#> [[3]]
#> [1] 120
#> 
#> [[4]]
#> [1] 180

# Get all y-values
yValues <- lapply(tableParam$formula$allPoints, function(point) {
  point$y
})
print(yValues)
#> [[1]]
#> [1] 0
#> 
#> [[2]]
#> [1] 1
#> 
#> [[3]]
#> [1] 2
#> 
#> [[4]]
#> [1] 3
```

The method `valueAt()` of the `TableFormula` returns the value of `y`
for the given `x`. If no entry exists for the `x`, the value `y` is
linearly interpolated between the two closest `x` values.

``` r
# Get the parameter defined by a table
tableParam <- getParameter("Organism|TableParameter", sim)

# Value at x = 60 is stored in the table
tableParam$formula$valueAt(60)
#> [1] 1

# Value at x = 90 is not in the table
tableParam$formula$valueAt(90)
#> [1] 1.5
```

## Changing table parameter values

Simply setting the value of a table-defined parameter using
`setParameterValues` will override the formula and make the parameter
constant.

``` r
# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 0.00e+00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: FALSE

# Set value to a constant. tableParam$isFixedValue is now TRUE
setParameterValues(tableParam, 10)
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE
```

To change the values of the table, a set of methods of the
`TableFormula` is available. The method `addPoints()` adds a set of x-y
values to the existing table. If trying to add a point with the x-value
already present in the table, an error is thrown:

``` r
tableParam <- getParameter("Organism|TableParameter", sim)
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE

# Add new points
tableParam$formula$addPoints(c(1, 2, 3), c(5, 6, 7))
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE

# Try to add points with existing x-values
tableParam$formula$addPoints(0, 1)
#> Error in `do.call()`:
#> ! Type:    OSPSuite.Core.Domain.ValuePointAlreadyExistsForPointException
#> Message: A point for x=0 was already added with y=1
#> Method:  Int32 AddPoint(OSPSuite.Core.Domain.Formulas.ValuePoint)
#> Stack trace:
#>    at OSPSuite.Core.Domain.Formulas.TableFormula.AddPoint(ValuePoint point)
#>    at OSPSuite.Core.Domain.Formulas.TableFormula.AddPoint(Double x, Double y)
#>    at InvokeStub_TableFormula.AddPoint(Object, Span`1)
#>    at System.Reflection.MethodBaseInvoker.InvokeWithFewArgs(Object obj, BindingFlags invokeAttr, Binder binder, Object[] parameters, CultureInfo culture)
```

To remove a point from the table, use the method `removePoint()`. It
remove a point if the x value is present in the table and has the
provided y.

``` r
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 0, y= 0, restartSolver= FALSE
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE

# Remove the point (0, 0)
tableParam$formula$removePoint(0, 0)
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE

# Try to remove the point (1, 1). Note that the value for x = 1 is x = 5 in the original table,
# and no point is removed.
tableParam$formula$removePoint(1, 1)
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE

# Try to remove a non-existing point (0, 1). No point is removed.
tableParam$formula$removePoint(1, 1)
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE
```

The `clearPoints()` method removes all points from the table, while
`setPoints()` method is a combination of clearing the table and adding
new points:

``` r
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 60, y= 1, restartSolver= FALSE
#>   x= 120, y= 2, restartSolver= FALSE
#>   x= 180, y= 3, restartSolver= FALSE
#>   • Value overrides formula: TRUE

tableParam$formula$setPoints(c(1, 2, 3, 4), c(5, 6, 7, 8))
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   x= 1, y= 5, restartSolver= FALSE
#>   x= 2, y= 6, restartSolver= FALSE
#>   x= 3, y= 7, restartSolver= FALSE
#>   x= 4, y= 8, restartSolver= FALSE
#>   • Value overrides formula: TRUE

tableParam$formula$clearPoints()
tableParam
#> <Parameter>
#>   • Quantity Type: Parameter
#>   • Path: Organism|TableParameter
#>   • Value: 10.00
#> 
#> ── Formula ──
#> 
#>   • isTable: TRUE
#>   • XDimension: Time
#>   • UseDerivedValues: FALSE
#> 
#> ── Table values ────────────────────────────────────────────────────────────────
#>   • Value overrides formula: TRUE
```
