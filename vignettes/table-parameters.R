## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)

## ----getTableFormulaValue-----------------------------------------------------
library(ospsuite)

# Load a simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
print(tableParam)

## ----getAllXYValues-----------------------------------------------------------
# Get the parameter defined by a table
tableParam <- getParameter("Organism|TableParameter", sim)
print(tableParam)

# Get all value points
tableParam$formula$allPoints

# Get all x-values
xValues <- lapply(tableParam$formula$allPoints, function(point) {
  point$x
})
print(xValues)

# Get all y-values
yValues <- lapply(tableParam$formula$allPoints, function(point) {
  point$y
})
print(yValues)

## ----valueAt------------------------------------------------------------------
# Get the parameter defined by a table
tableParam <- getParameter("Organism|TableParameter", sim)

# Value at x = 60 is stored in the table
tableParam$formula$valueAt(60)

# Value at x = 90 is not in the table
tableParam$formula$valueAt(90)

## ----setTableToConstant-------------------------------------------------------

# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
tableParam

# Set value to a constant. tableParam$isFixedValue is now TRUE
setParameterValues(tableParam, 10)
tableParam

## ----addPoints, error=TRUE----------------------------------------------------
tableParam <- getParameter("Organism|TableParameter", sim)
tableParam

# Add new points
tableParam$formula$addPoints(c(1, 2, 3), c(5, 6, 7))
tableParam

# Try to add points with existing x-values
tableParam$formula$addPoints(0, 1)

## ----removePoint--------------------------------------------------------------
tableParam

# Remove the point (0, 0)
tableParam$formula$removePoint(0, 0)
tableParam

# Try to remove the point (1, 1). Note that the value for x = 1 is x = 5 in the original table,
# and no point is removed.
tableParam$formula$removePoint(1, 1)
tableParam

# Try to remove a non-existing point (0, 1). No point is removed.
tableParam$formula$removePoint(1, 1)
tableParam

## ----setPoints----------------------------------------------------------------
tableParam

tableParam$formula$setPoints(c(1, 2, 3, 4), c(5, 6, 7, 8))
tableParam

tableParam$formula$clearPoints()
tableParam

