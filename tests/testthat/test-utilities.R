context("netEnumName")
enumType <- "OSPSuite.Core.Domain.Data.AuxiliaryType"

test_that("It returns the correct name of an enum entry", {
  expect_equal(netEnumName(enumType = enumType, 1L), "ArithmeticStdDev")
})

test_that("It returns NULL if the entry does not exist", {
  expect_equal(netEnumName(enumType = enumType, 5L), NULL)
})

context("createConfigurationForFile")
filePath <-getTestDataFilePath("CompiledDataSet.xlsx")

test_that("It can create a DataImporterConfiguration from a XLS file", {
  importerConfiguration <- createConfigurationForFile(filePath, "TestSheet_1")
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
