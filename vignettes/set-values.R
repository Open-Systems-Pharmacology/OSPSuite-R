## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE
)

## ----getConstantValue---------------------------------------------------------
library(ospsuite)

# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

# Get the parameter "Age" of the Organism
ageParam <- getParameter("Organism|Age", sim)
print(ageParam)
ageParam$isConstant

## ----getDistributedValue------------------------------------------------------
# Get the parameter "Volume" of the Liver
liverVolume <- getParameter("Organism|Liver|Volume", sim)
print(liverVolume)
liverVolume$isDistributed

## ----getExplicitFormulaValue--------------------------------------------------
# Get the parameter "Volume" of the Liver interstital
liverIntVolume <- getParameter("Organism|Liver|Interstitial|Volume", sim)
print(liverIntVolume)
liverIntVolume$formulaString

## ----getTableFormulaValue-----------------------------------------------------
# Get the parameter defined by a table.
tableParam <- getParameter("Organism|TableParameter", sim)
print(tableParam)

## ----getStateVariableValue----------------------------------------------------
# Get the parameter defined by a state variable.
stateVariableParam <- getParameter("Organism|StateVariable_Parameter", sim)
print(stateVariableParam)

# `value` refers to the initial value of the parameter
stateVariableParam$value
# `rhsFormula` is the right hand side of the parameter
stateVariableParam$rhsFormula

## ----changeDose---------------------------------------------------------------
# Get the parameter Dose
doseParamPath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
doseParam <- getParameter(doseParamPath, sim)
print(doseParam)

# Change the dose to 350mg. The value has to be converted to base unit, first
newValue <- toBaseUnit(quantity = doseParam, values = 350, unit = "mg")
setParameterValues(parameters = doseParam, values = newValue)
print(doseParam)

## ----scaleParameter-----------------------------------------------------------
doseParamPath <- "Applications|IV 250mg 10min|Application_1|ProtocolSchemaItem|Dose"
doseParam <- getParameter(doseParamPath, sim)
print(doseParam)

# Double the dose
scaleParameterValues(doseParam, factor = 2)
print(doseParam)

# Half the dose
scaleParameterValues(doseParam, factor = 0.5)
print(doseParam)

## ----setExplicitFormulaValue--------------------------------------------------
# Get the parameter "Volume" of the Liver interstital
liverIntVolume <- getParameter("Organism|Liver|Interstitial|Volume", sim)
print(liverIntVolume)

# isFixedValue is FALSE as the parameter is defined by its formula
liverIntVolume$isFixedValue

setParameterValues(liverIntVolume, 1)
print(liverIntVolume)
# isFixedValue is TRUE as the value of parameter is overridden by the constant value
liverIntVolume$isFixedValue

## ----resetExplicitFormulaValue------------------------------------------------
print(liverIntVolume)

# isFixedValue is TRUE as the value of parameter is overridden by the constant value
liverIntVolume$isFixedValue

liverIntVolume$reset()
print(liverIntVolume)
liverIntVolume$isFixedValue

## ----setStateVariableValue----------------------------------------------------
# Get the parameter defined by a state variable.
stateVariableParam <- getParameter("Organism|StateVariable_Parameter", sim)
print(stateVariableParam)

# Setting its value only changes the initial value
setParameterValues(stateVariableParam, 10)
print(stateVariableParam)

# Switching the RHS formula off
stateVariableParam$isStateVariable <- FALSE
print(stateVariableParam)

# Switching it on is not supported
stateVariableParam$isStateVariable <- TRUE

## ----changeInitialValue-------------------------------------------------------
# Get objects representing the molecule Aciclovir in all containers
allAciclovirMolecules <- getAllMoleculesMatching("Organism|**|Aciclovir", sim)

# Set initial values to 10 µmol in all containers
setMoleculeInitialValues(allAciclovirMolecules, rep(10, length(allAciclovirMolecules)))

