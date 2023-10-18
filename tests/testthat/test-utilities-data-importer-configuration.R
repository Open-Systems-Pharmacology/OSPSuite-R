# createImporterConfigurationForFile
filePath <- getTestDataFilePath("CompiledDataSet.xlsx")

test_that("It can create a DataImporterConfiguration from a XLS file", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- createImporterConfigurationForFile(filePath = filePath, sheet = "TestSheet_1")
  expect_equal(importerConfiguration$timeColumn, "Time [h]")
  expect_equal(importerConfiguration$errorColumn, "Error [ng/ml]")
  expect_equal(importerConfiguration$measurementColumn, "Concentration (mass)[ng/ml]")
  expect_equal(importerConfiguration$errorType, DataErrorType$ArithmeticStdDev)
  expect_equal(importerConfiguration$errorUnit, "ng/ml")
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(importerConfiguration$measurementUnit, "ng/ml")
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$isTimeUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$sheets, character())
})

test_that("It can create a DataImporterConfiguration from a XLS file when dimensions
          and units can not be retrieved, they can be set afterwards", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- createImporterConfigurationForFile(filePath = filePath, sheet = "DefaultConfig")
  expect_equal(importerConfiguration$timeColumn, "Time")
  expect_equal(importerConfiguration$errorColumn, "Error")
  expect_equal(importerConfiguration$measurementColumn, "Measurement")
  expect_equal(importerConfiguration$errorType, DataErrorType$GeometricStdDev)
  expect_equal(importerConfiguration$errorUnit, "?")
  expect_equal(importerConfiguration$measurementDimension, NULL)
  expect_equal(importerConfiguration$measurementUnit, "?")
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$isTimeUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$sheets, character())

  importerConfiguration$measurementDimension <- ospDimensions$Hertz
  importerConfiguration$measurementUnit <- ospUnits$Hertz$`1/s`
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$Hertz)
  expect_equal(importerConfiguration$measurementUnit, ospUnits$Hertz$`1/s`)
})

test_that("It throws an error when the file sheet has wrong format", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux
  expect_error(createImporterConfigurationForFile(filePath, "MetaInfo"))
})

# DataImporterConfiguration from file

test_that("it can load a data importer configuration", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  configurationPath <- getTestDataFilePath("dataImporterConfiguration.xml")
  importerConfiguration <- loadDataImporterConfiguration(configurationPath)
  expect_equal(importerConfiguration$timeColumn, "Time [h]")
  expect_equal(importerConfiguration$errorColumn, "Error [ng/ml]")
  expect_equal(importerConfiguration$measurementColumn, "Concentration (mass)[ng/ml]")
  expect_equal(importerConfiguration$errorType, DataErrorType$ArithmeticStdDev)
  expect_equal(importerConfiguration$errorUnit, "ng/ml")
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(importerConfiguration$measurementUnit, "ng/ml")
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$isTimeUnitFromColumn, FALSE)
  # just checking for the number of grouping columns here as the sequence of column
  # names is not obvious
  expect_equal(length(importerConfiguration$groupingColumns), 10)
  expect_equal(importerConfiguration$sheets, character())

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can load a data importer configuration with units from columns", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  configurationPath <- getTestDataFilePath("dataImporterConfiguration_UnitFromColumn.xml")
  importerConfiguration <- loadDataImporterConfiguration(configurationPath)
  expect_equal(importerConfiguration$timeColumn, "Time [h]")
  expect_equal(importerConfiguration$errorColumn, "Error [ng/ml]")
  expect_equal(importerConfiguration$measurementColumn, "Concentration (mass)[ng/ml]")
  expect_equal(importerConfiguration$errorType, DataErrorType$ArithmeticStdDev)
  expect_equal(importerConfiguration$errorUnit, "measurementUnit")
  expect_equal(importerConfiguration$measurementDimension, NULL)
  expect_equal(importerConfiguration$measurementUnit, "measurementUnit")
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, TRUE)
  expect_equal(importerConfiguration$timeUnit, "TimeUnit")
  expect_equal(importerConfiguration$isTimeUnitFromColumn, TRUE)
  # just checking for the number of grouping columns here as the sequence of column
  # names is not obvious
  expect_equal(length(importerConfiguration$groupingColumns), 11)
  expect_equal(importerConfiguration$sheets, c("UnitsFromColumn", "UnitsFromColumn_secondSheet"))

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})
