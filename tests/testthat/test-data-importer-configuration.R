# New DataImporterConfiguration

test_that("it can create a new data importer configuration", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$timeColumn, "Time")
  expect_equal(importerConfiguration$errorColumn, NULL)
  expect_equal(importerConfiguration$measurementColumn, "Measurement")
  expect_equal(importerConfiguration$errorType, NULL)
  expect_equal(importerConfiguration$errorUnit, NULL)
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$`Concentration (molar)`)
  expect_equal(importerConfiguration$measurementUnit, .encodeUnit("µmol/l"))
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$isTimeUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$groupingColumns, character())
  expect_equal(importerConfiguration$sheets, character())
  expect_equal(importerConfiguration$namingPattern, "{Source}.{Sheet}")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can get and set time column name", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$timeColumn <- "foo"
  expect_equal(importerConfiguration$timeColumn, "foo")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can get and set time unit", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$timeUnit <- "foo")
  importerConfiguration$timeUnit <- "min"
  expect_equal(importerConfiguration$timeUnit, "min")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can set time unit from column and change column name", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$isTimeUnitFromColumn <- TRUE
  expect_equal(importerConfiguration$isTimeUnitFromColumn, TRUE)
  importerConfiguration$timeUnit <- "foo"
  expect_equal(importerConfiguration$timeUnit, "foo")

  importerConfiguration$isTimeUnitFromColumn <- FALSE
  expect_equal(importerConfiguration$timeUnit, "h")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can get and set measurement column name", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$measurementColumn <- "foo"
  expect_equal(importerConfiguration$measurementColumn, "foo")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can get and set measurement unit", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$measurementUnit <- "foo")
  importerConfiguration$measurementUnit <- "mol/l"
  expect_equal(importerConfiguration$measurementUnit, "mol/l")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can set measurement unit from column and change column name", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$isMeasurementUnitFromColumn <- TRUE
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, TRUE)
  importerConfiguration$measurementUnit <- "foo"
  expect_equal(importerConfiguration$measurementUnit, "foo")
  expect_equal(importerConfiguration$measurementDimension, NULL)

  importerConfiguration$isMeasurementUnitFromColumn <- FALSE
  
  #TODO enable tests!
#  expect_equal(importerConfiguration$measurementDimension, ospDimensions$`Concentration (molar)`)
#  expect_equal(importerConfiguration$measurementUnit, .encodeUnit("µmol/l"))

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can add an error column", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  expect_equal(importerConfiguration$errorColumn, "Error")
  expect_equal(importerConfiguration$errorType, DataErrorType$ArithmeticStdDev)
  expect_equal(importerConfiguration$errorUnit, importerConfiguration$measurementUnit)
})

test_that("it can remove an error column", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$errorColumn <- NULL
  expect_equal(importerConfiguration$errorColumn, NULL)
  expect_equal(importerConfiguration$errorType, NULL)
  expect_equal(importerConfiguration$errorUnit, NULL)
})

test_that("it can change measurement dimension without error", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$measurementDimension <- "foo")
  importerConfiguration$measurementDimension <- ospDimensions$`Concentration (mass)`
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(importerConfiguration$measurementUnit, "kg/l")
  expect_equal(importerConfiguration$errorUnit, NULL)

  importerConfiguration$isMeasurementUnitFromColumn <- TRUE
  importerConfiguration$measurementUnit <- "measCol"
  expect_equal(importerConfiguration$measurementUnit, "measCol")
  importerConfiguration$errorUnit <- "errCol"
  expect_equal(importerConfiguration$errorUnit, NULL)

  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, TRUE)
  importerConfiguration$measurementUnit <- "foo"
  expect_equal(importerConfiguration$measurementUnit, "foo")
  expect_equal(importerConfiguration$measurementDimension, NULL)

  importerConfiguration$isMeasurementUnitFromColumn <- FALSE
  expect_equal(importerConfiguration$measurementDimension, ospDimensions$`Concentration (mass)`)
  expect_equal(importerConfiguration$measurementUnit, "kg/l")
  expect_equal(importerConfiguration$errorUnit, NULL)

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can change measurement dimension with error", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$measurementDimension <- ospDimensions$`Concentration (mass)`
  expect_equal(importerConfiguration$measurementUnit, "kg/l")
  expect_equal(importerConfiguration$errorUnit, "kg/l")

  importerConfiguration$isMeasurementUnitFromColumn <- TRUE
  importerConfiguration$errorUnit <- "errCol"
  expect_equal(importerConfiguration$errorUnit, "errCol")

  importerConfiguration$measurementUnit <- "foo"
  expect_equal(importerConfiguration$measurementUnit, "foo")
  expect_equal(importerConfiguration$measurementDimension, NULL)

  importerConfiguration$isMeasurementUnitFromColumn <- FALSE
  expect_equal(importerConfiguration$errorUnit, "kg/l")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can change error type", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  expect_error(importerConfiguration$errorType <- "foo")
  importerConfiguration$errorType <- DataErrorType$GeometricStdDev
  expect_equal(importerConfiguration$errorType, DataErrorType$GeometricStdDev)

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can add grouping columns", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$groupingColumns, character())
  importerConfiguration$addGroupingColumn("foo")
  expect_equal(importerConfiguration$groupingColumns, "foo")
  # adding the same group does not create duplicates
  importerConfiguration$addGroupingColumn("foo")
  expect_equal(importerConfiguration$groupingColumns, "foo")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it does not fail when trying to remove a grouping column that is not present", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$removeGroupingColumn(column = "tata"), regexp = NA)

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can remove grouping columns", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$addGroupingColumn("foo")
  importerConfiguration$addGroupingColumn("tata")
  expect_equal(importerConfiguration$groupingColumns, c("foo", "tata"))
  importerConfiguration$removeGroupingColumn(column = "tata")
  expect_equal(importerConfiguration$groupingColumns, "foo")

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can add sheet", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$sheets <- "S1"
  expect_equal(importerConfiguration$sheets, "S1")
  importerConfiguration$sheets <- "S2"
  expect_equal(importerConfiguration$sheets, "S2")
  importerConfiguration$sheets <- c("S1", "S2")
  expect_equal(importerConfiguration$sheets, c("S1", "S2"))

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can remove all sheets", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$sheets <- NULL
  expect_equal(importerConfiguration$sheets, character())

  importerConfiguration$sheets <- c("S1", "S2")
  expect_equal(importerConfiguration$sheets, c("S1", "S2"))
  importerConfiguration$sheets <- c()
  expect_equal(importerConfiguration$sheets, character())

  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})

test_that("it can change naming pattern", {
  skip_on_os("linux") # TODO enable again as soon as npoi works under linux

  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$namingPattern, "{Source}.{Sheet}")
  importerConfiguration$namingPattern <- "foo"
  expect_equal(importerConfiguration$namingPattern, "foo")
  expect_error(capture.output(print(importerConfiguration)), regexp = NA)
})
