## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----newDataSet---------------------------------------------------------------
library(ospsuite)

# Create an empty data set
dataSet <- DataSet$new("My data set")

## ----setValues----------------------------------------------------------------
dataSet$setValues(
  xValues = c(1, 2, 3, 4),
  yValues = c(0, 0.1, 0.6, 10),
  yErrorValues = c(0.001, 0.001, 0.1, 1)
)

print(dataSet)

## ----setDimensionUnit---------------------------------------------------------
# Print x, y, and error values
dataSet$xValues
dataSet$yValues
dataSet$yErrorValues

# Change the unit of x-values
dataSet$xUnit <- ospUnits$Time$min
# Print the x values - they did not change
dataSet$xValues

# Change dimension of y-values
dataSet$yDimension <- ospDimensions$Amount

print(dataSet)

# Change the units of y values and error values - they are now different!
dataSet$yUnit <- ospUnits$Amount$mol
dataSet$yErrorUnit <- ospUnits$Amount$pmol

print(dataSet)

## ----setErrorType-------------------------------------------------------------
# Default error type is "ArithmeticStdDev"
dataSet$yErrorType

# Change error type to geometric
dataSet$yErrorType <- DataErrorType$GeometricStdDev
# Error unit is "Unitless" for dimension "Fraction".
dataSet$yErrorUnit

# Changing error type to arithmetic will set the dimension and unit of the error
# to the same dimension and unit as the y values
dataSet$yErrorType <- DataErrorType$ArithmeticStdDev

print(dataSet)

## ----addMetaData--------------------------------------------------------------
# Add new meta data entries
dataSet$addMetaData(
  name = "Molecule",
  value = "Aciclovir"
)
dataSet$addMetaData(
  name = "Organ",
  value = "Muscle"
)

# Print meta data of the DataSet
print(dataSet$metaData)

## ----dataSetToDataFrame-------------------------------------------------------
# Create a second data set
dataSet2 <- DataSet$new(name = "Second data set")
dataSet2$setValues(
  xValues = c(1, 2, 3, 4, 5),
  yValues = c(1, 0, 5, 8, 0.1)
)

# Convert data sets to a tibble
myTibble <- dataSetToTibble(dataSets = c(dataSet, dataSet2))

print(myTibble)

## ----loadDataSetFromPKML------------------------------------------------------
# Load a data set from PKML
filePath <- system.file("extdata", "ObsDataAciclovir_1.pkml", package = "ospsuite")

dataSet <- loadDataSetFromPKML(filePath = filePath)

print(dataSet)

## ----loadDataImporterConfiguration--------------------------------------------
# Load a configuration from xml file
filePath <- system.file("extdata", "dataImporterConfiguration.xml", package = "ospsuite")
importerConfiguration <- loadDataImporterConfiguration(configurationFilePath = filePath)

print(importerConfiguration)

## ----createDataImporterConfigurationFor---------------------------------------
# Excel file
excelFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
sheetName <- "TestSheet_1"

# Create importer configuration for the excel sheet
importerConfiguration_guessed <- createImporterConfigurationForFile(
  filePath = excelFilePath,
  sheet = sheetName
)

print(importerConfiguration)

## ----loadSheets---------------------------------------------------------------
# Excel file
excelFilePath <- system.file("extdata", "CompiledDataSet.xlsx", package = "ospsuite")
sheetName <- "TestSheet_1"

# Create importer configuration for the excel sheet
importerConfiguration_guessed <- createImporterConfigurationForFile(
  filePath = excelFilePath,
  sheet = sheetName
)
# Add sheet names to the configuration
importerConfiguration_guessed$sheets <- c("TestSheet_1", "TestSheet_1_withMW")

# Load data
dataSets <- loadDataSetsFromExcel(
  xlsFilePath = excelFilePath,
  importerConfigurationOrPath = importerConfiguration_guessed
)

