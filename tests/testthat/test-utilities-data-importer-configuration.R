context("createImporterConfigurationForFile")
filePath <- getExtDataFilePath("CompiledDataSet.xlsx")

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
  expect_equal(importerConfiguration$measurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$timeUnitFromColumn, FALSE)
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
  expect_equal(importerConfiguration$measurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$timeUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$sheets, character())

  importerConfiguration$measurementDimension <- ospDimensions$Hertz
  importerConfiguration$measurementUnit<- ospUnits$Hertz$`1/s`
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$Hertz)
  expect_equal(importerConfiguration$measurementUnit, ospUnits$Hertz$`1/s`)
})

test_that("It throws an error when the file sheet has wrong format", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux
  expect_error(createImporterConfigurationForFile(filePath, "MetaInfo"))
})
