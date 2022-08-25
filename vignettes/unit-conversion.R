## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dimension----------------------------------------------------------------
library(ospsuite)

# Load a simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter volume of the liver.
livParam <- getParameter("Organism|Liver|Volume", sim)
print(livParam)

# Dimension of the parameter
livParam$dimension

## ----allUnits-----------------------------------------------------------------
# Dimension of the parameter
livParam$dimension

# Units of the parameter
livParam$allUnits

## ----unitConversion-----------------------------------------------------------
# Get the BMI parameter
bmiParam <- getParameter("Organism|BMI", sim)
print(bmiParam)

# Print the base and the default display units
bmiParam$unit
bmiParam$displayUnit

# Convert the value from the base into the default display unit
toDisplayUnit(quantity = bmiParam, values = bmiParam$value)

# Convert the value to the base unit, that can be used. e.g. for setting new parameter value
toBaseUnit(quantity = bmiParam, values = 30, unit = "kg/m²")

liverVolume <- getParameter("Organism|Liver|Volume", sim)
print(liverVolume)

liverVolume$allUnits

# Convert from base volume unit to µl
toUnit(quantity = liverVolume, values = c(1, 2, 3, 4), targetUnit = "ml")

