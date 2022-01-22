context("loadDataSetFromPKML")

test_that("It can load a valid observed data file and create a DataSet object", {
  file <- getTestDataFilePath("obs_data.pkml")
  dataSet <- loadDataSetFromPKML(file)

  expect_true(isOfType(dataSet, DataSet))
})

context("dataSetToDataFrame")

dataSet <- DataSet$new()

test_that("It can convert an empty data set", {
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = character(0), xValues = numeric(0), yValues = numeric(0), yErrorValues = numeric(0),
      xDimension = character(0), xUnit = character(0), yDimension = character(0),
      yUnit = character(0), yErrorType = numeric(0), yErrorUnit = numeric(0), molWeight = numeric(0),
      lloq = numeric(0)
    )
  )
})

test_that("It can convert a data set with xValues and yValues set by setValues, other fields are default", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = rep("", 5), xValues = dataSet$xValues, yValues = dataSet$yValues, yErrorValues = rep(NA_real_, 5),
      xDimension = rep(dataSet$xDimension, 5), xUnit = rep(dataSet$xUnit, 5),
      yDimension = rep(dataSet$yDimension, 5), yUnit = rep(dataSet$yUnit, 5),
      yErrorType = rep(NA_real_, 5), yErrorUnit = rep(NA_real_, 5), molWeight = rep(NA_real_, 5), lloq = rep(NA_real_, 5)
    )
  )
})

test_that("It can convert a data set with only non-empty fields, except for metaData", {
  dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50), yErrorValues = c(0, 1, 2, 3, 0))
  dataSet$name <- "Data1"
  dataSet$molWeight <- 123
  dataSet$LLOQ <- 0.2
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = rep(dataSet$name, 5), xValues = dataSet$xValues,
      yValues = dataSet$yValues, yErrorValues = dataSet$yErrorValues,
      xDimension = rep(dataSet$xDimension, 5), xUnit = rep(dataSet$xUnit, 5),
      yDimension = rep(dataSet$yDimension, 5), yUnit = rep(dataSet$yUnit, 5),
      yErrorType = rep(dataSet$yErrorType, 5), yErrorUnit = rep(dataSet$yErrorUnit, 5),
      molWeight = rep(dataSet$molWeight, 5), lloq = rep(dataSet$LLOQ, 5)
    )
  )
})

test_that("It can convert a data set with metaData", {
  skip_on_os("linux") # TODO enable again as soon as NPOI runs under Linux; s. https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/647

  dataSet$addMetaData("Organ", "Blood")
  expect_equal(
    dataSetToDataFrame(dataSet),
    data.frame(
      name = rep(dataSet$name, 5), xValues = dataSet$xValues,
      yValues = dataSet$yValues, yErrorValues = dataSet$yErrorValues,
      xDimension = rep(dataSet$xDimension, 5), xUnit = rep(dataSet$xUnit, 5),
      yDimension = rep(dataSet$yDimension, 5), yUnit = rep(dataSet$yUnit, 5),
      yErrorType = rep(dataSet$yErrorType, 5), yErrorUnit = rep(dataSet$yErrorUnit, 5),
      molWeight = rep(dataSet$molWeight, 5), lloq = rep(dataSet$LLOQ, 5),
      Organ = rep("Blood", 5)
    )
  )
})

test_that("It can convert a list of data sets", {
  skip_on_os("linux") # TODO enable again as soon as NPOI runs under Linux; s. https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/647

  dataSet2 <- DataSet$new()
  dataSet2$setValues(xValues = c(6, 7, 8), yValues = c(11, 21, 31))
  dataSet2$molWeight <- 456
  dataSet2$addMetaData("Compartment", "Plasma")

  expect_equal(
    dataSetToDataFrame(list(dataSet, dataSet2)),
    data.frame(
      name = c(rep(dataSet$name, 5), rep("", 3)), xValues = c(dataSet$xValues, dataSet2$xValues),
      yValues = c(dataSet$yValues, dataSet2$yValues), yErrorValues = c(dataSet$yErrorValues, rep(NA, 3)),
      xDimension = c(rep(dataSet$xDimension, 5), rep(dataSet2$xDimension, 3)),
      xUnit = c(rep(dataSet$xUnit, 5), rep(dataSet2$xUnit, 3)),
      yDimension = c(rep(dataSet$yDimension, 5), rep(dataSet2$yDimension, 3)),
      yUnit = c(rep(dataSet$yUnit, 5), rep(dataSet2$yUnit, 3)),
      yErrorType = c(rep(dataSet$yErrorType, 5), rep(NA_character_, 3)),
      yErrorUnit = c(rep(dataSet$yErrorUnit, 5), rep(NA_character_, 3)),
      molWeight = c(rep(dataSet$molWeight, 5), rep(dataSet2$molWeight, 3)),
      lloq = c(rep(dataSet$LLOQ, 5), rep(NA, 3)),
      Organ = c(rep("Blood", 5), rep(NA, 3)), Compartment = c(rep(NA, 5), rep("Plasma", 3))
    )
  )
})

context("saveDataSetToPKML")

obsDataFile <- getTestDataFilePath("obs_data.pkml")

xValues <- c(
  1.79999995231628, 4.86999988555908, 10.1999998092651,
  30, 60, 120, 240
)
yValues <- c(
  0.00100999997254547, 0.000830000013163357, 0.00073000000488932,
  0.000279999995411728, 0.000119999996051057, 3.0499998360245E-05, 5.9299999806417E-06
)
yError <- c(
  1.15000000278087, 1.08999995518388, 1.12999998691521, 1.21999994462385, 1.4099999816608,
  1.18000002657936, 1.48000003719062
)
metaData <- list(
  Source = "C:\\temp\\RanorexTestData\\ObservedData.xlsx",
  Sheet = "Tabelle1",
  DoubleValue = 5,
  StringValues = "hello",
  IntegerValue = 4
)

test_that("it can save the data set as pkml", {
  dataSet <- loadDataSetFromPKML(obsDataFile)
  filePath <- getTestDataFilePath("obs_data_save.pkml")

  saveDataSetToPKML(dataSet = dataSet, filePath = filePath)
  # load the saved file and check everything is correct
  dataSet <- loadDataSetFromPKML(filePath)

  expect_equal(dataSet$xValues, xValues)
  expect_equal(dataSet$yValues, yValues)
  expect_equal(dataSet$yErrorValues, yError)
  expect_equal(dataSet$metaData, metaData)
  expect_equal(dataSet$name, "ObservedData")
  expect_equal(dataSet$xDimension, ospDimensions$Time)
  expect_equal(dataSet$xUnit, ospUnits$Time$min)
  expect_equal(dataSet$yDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(dataSet$yUnit, "mg/l")
  expect_equal(dataSet$yErrorType, DataErrorType$ArithmeticStdDev)
  expect_equal(dataSet$yErrorUnit, "mg/l")
  expect_equal(dataSet$yErrorValues, yError)
  expect_error(capture.output(print(dataSet)), regexp = NA)

  # remove the temp file
  capture.output(file.remove(filePath))
})

context("loadDataSetsFromExcel")
configurationPath <- getTestDataFilePath("dataImporterConfiguration_noSheets.xml")
xlsFilePath <- getTestDataFilePath("CompiledDataSet_oneSheet.xlsx")
importerConfiguration <- DataImporterConfiguration$new(configurationPath)

test_that("it returns an empty list when loading from file with one sheet without
          sheet definition in configuration and importAllSheets == FALSE", {
  skip_on_os("linux") # TODO enable again as soon as NPOI runs under Linux; s. https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/647

  expect_named(loadDataSetsFromExcel(xlsFilePath = xlsFilePath, importerConfiguration = importerConfiguration), character())
})

test_that("it can load when loading from file with one sheet without
          sheet definition in configuration and importAllSheets == FALSE", {
  skip_on_os("linux") # TODO enable again as soon as NPOI runs under Linux; s. https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/647

  dataSets <- loadDataSetsFromExcel(xlsFilePath = xlsFilePath, importerConfiguration = importerConfiguration, importAllSheets = TRUE)
  expect_true(isOfType(dataSets, DataSet))
  expect_equal(length(dataSets), 4)
})

test_that("it can convert DataSets loaded from excel to data.frame", {
  skip_on_os("linux") # TODO enable again as soon as NPOI runs under Linux; s. https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/647

  dataSets <- loadDataSetsFromExcel(xlsFilePath = xlsFilePath, importerConfiguration = importerConfiguration, importAllSheets = TRUE)
  dataSetsFrame <- dataSetToDataFrame(dataSets)
  expect_equal(names(dataSetsFrame), c(
    "name",
    "xValues",
    "yValues",
    "yErrorValues",
    "xDimension",
    "xUnit",
    "yDimension",
    "yUnit",
    "yErrorType",
    "yErrorUnit",
    "molWeight",
    "lloq",
    "Source",
    "Sheet",
    "Study Id",
    "Organ",
    "Compartment",
    "Species",
    "Gender",
    "Molecule",
    "Route",
    "Subject Id",
    "Dose",
    "Group Id"
  ))
})
