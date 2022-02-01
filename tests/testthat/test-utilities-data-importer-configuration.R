context("createImporterConfigurationForFile")
filePath <- getTestDataFilePath("CompiledDataSet.xlsx")

test_that("It can create a DataImporterConfiguration from a XLS file", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- createImporterConfigurationForFile(filePath, "TestSheet_1")
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
