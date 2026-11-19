# New DataImporterConfiguration

test_that("it can create a new data importer configuration", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$timeColumn, "Time")
  expect_equal(importerConfiguration$errorColumn, NULL)
  expect_equal(importerConfiguration$measurementColumn, "Measurement")
  expect_equal(importerConfiguration$errorType, NULL)
  expect_equal(importerConfiguration$errorUnit, NULL)
  expect_equal(
    importerConfiguration$measurementDimension,
    ospDimensions$`Concentration (molar)`
  )
  expect_equal(importerConfiguration$measurementUnit, .encodeUnit("µmol/l"))
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$timeUnit, ospUnits$Time$h)
  expect_equal(importerConfiguration$isTimeUnitFromColumn, FALSE)
  expect_equal(importerConfiguration$groupingColumns, character())
  expect_equal(importerConfiguration$sheets, character())
  expect_equal(importerConfiguration$namingPattern, "{Source}.{Sheet}")
})

test_that("It can print the configuration", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$addGroupingColumn("foo")
  importerConfiguration$addGroupingColumn("tata")

  expect_snapshot(print(importerConfiguration))
})

test_that("it can get and set time column name", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$timeColumn <- "foo"
  expect_equal(importerConfiguration$timeColumn, "foo")
})

test_that("it can get and set time unit", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$timeUnit <- "foo")
  importerConfiguration$timeUnit <- "min"
  expect_equal(importerConfiguration$timeUnit, "min")
})

test_that("it can set time unit from column and change column name", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$isTimeUnitFromColumn <- TRUE
  expect_equal(importerConfiguration$isTimeUnitFromColumn, TRUE)
  importerConfiguration$timeUnit <- "foo"
  expect_equal(importerConfiguration$timeUnit, "foo")

  importerConfiguration$isTimeUnitFromColumn <- FALSE
  expect_equal(importerConfiguration$timeUnit, "h")
})

test_that("it can get and set measurement column name", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$measurementColumn <- "foo"
  expect_equal(importerConfiguration$measurementColumn, "foo")
})

test_that("it can get and set measurement unit", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$measurementUnit <- "foo")
  importerConfiguration$measurementUnit <- "mol/l"
  expect_equal(importerConfiguration$measurementUnit, "mol/l")
})

test_that("it can set measurement unit from column and change column name", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$isMeasurementUnitFromColumn <- TRUE
  expect_equal(importerConfiguration$isMeasurementUnitFromColumn, TRUE)
  importerConfiguration$measurementUnit <- "foo"
  expect_equal(importerConfiguration$measurementUnit, "foo")
  expect_equal(importerConfiguration$measurementDimension, NULL)

  importerConfiguration$isMeasurementUnitFromColumn <- FALSE

  expect_equal(
    importerConfiguration$measurementDimension,
    ospDimensions$`Concentration (molar)`
  )
  expect_equal(importerConfiguration$measurementUnit, .encodeUnit("µmol/l"))
})

test_that("it can add an error column", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  expect_equal(importerConfiguration$errorColumn, "Error")
  expect_equal(importerConfiguration$errorType, DataErrorType$ArithmeticStdDev)
  expect_equal(
    importerConfiguration$errorUnit,
    importerConfiguration$measurementUnit
  )
})

test_that("it can remove an error column", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$errorColumn <- NULL
  expect_equal(importerConfiguration$errorColumn, NULL)
  expect_equal(importerConfiguration$errorType, NULL)
  expect_equal(importerConfiguration$errorUnit, NULL)
})

test_that("it can change measurement dimension without error", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(importerConfiguration$measurementDimension <- "foo")
  importerConfiguration$measurementDimension <- ospDimensions$`Concentration (mass)`
  expect_equal(
    importerConfiguration$measurementDimension,
    ospDimensions$`Concentration (mass)`
  )
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
  expect_equal(
    importerConfiguration$measurementDimension,
    ospDimensions$`Concentration (mass)`
  )
  expect_equal(importerConfiguration$measurementUnit, "kg/l")
  expect_equal(importerConfiguration$errorUnit, NULL)
})

test_that("it can change measurement dimension with error", {
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
})

test_that("it can change error type", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  expect_error(importerConfiguration$errorType <- "foo")
  importerConfiguration$errorType <- DataErrorType$GeometricStdDev
  expect_equal(importerConfiguration$errorType, DataErrorType$GeometricStdDev)
})

test_that("it can add grouping columns", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$groupingColumns, character())
  importerConfiguration$addGroupingColumn("foo")
  expect_equal(importerConfiguration$groupingColumns, "foo")
  # adding the same group does not create duplicates
  importerConfiguration$addGroupingColumn("foo")
  expect_equal(importerConfiguration$groupingColumns, "foo")
})

test_that("it does not fail when trying to remove a grouping column that is not present", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_error(
    importerConfiguration$removeGroupingColumn(column = "tata"),
    regexp = NA
  )
})

test_that("it can remove grouping columns", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$addGroupingColumn("foo")
  importerConfiguration$addGroupingColumn("tata")
  expect_equal(importerConfiguration$groupingColumns, c("foo", "tata"))
  importerConfiguration$removeGroupingColumn(column = "tata")
  expect_equal(importerConfiguration$groupingColumns, "foo")
})

test_that("it can add sheet", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$sheets <- "S1"
  expect_equal(importerConfiguration$sheets, "S1")
  importerConfiguration$sheets <- "S2"
  expect_equal(importerConfiguration$sheets, "S2")
  importerConfiguration$sheets <- c("S1", "S2")
  expect_equal(importerConfiguration$sheets, c("S1", "S2"))
})

test_that("it can remove all sheets", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$sheets <- NULL
  expect_equal(importerConfiguration$sheets, character())

  importerConfiguration$sheets <- c("S1", "S2")
  expect_equal(importerConfiguration$sheets, c("S1", "S2"))
  importerConfiguration$sheets <- c()
  expect_equal(importerConfiguration$sheets, character())
})

test_that("it can change naming pattern", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$namingPattern, "{Source}.{Sheet}")
  importerConfiguration$namingPattern <- "foo"
  expect_equal(importerConfiguration$namingPattern, "foo")
})

test_that("it can get and set the lloq column", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_equal(importerConfiguration$lloqColumn, NULL)
  importerConfiguration$lloqColumn <- "LLOQ"
  expect_equal(importerConfiguration$lloqColumn, "LLOQ")
  importerConfiguration$lloqColumn <- NULL
  expect_equal(importerConfiguration$lloqColumn, NULL)
})

# Error type from a column ---------------------------------------------------

test_that("it can get and set the error type column", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  expect_null(importerConfiguration$errorTypeColumn)
  importerConfiguration$errorTypeColumn <- "Error type"
  expect_equal(importerConfiguration$errorTypeColumn, "Error type")
  importerConfiguration$errorTypeColumn <- NULL
  expect_null(importerConfiguration$errorTypeColumn)
})

test_that("setting the error type column to an empty string removes it", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$errorTypeColumn <- "Error type"
  importerConfiguration$errorTypeColumn <- ""
  expect_null(importerConfiguration$errorTypeColumn)
})

test_that("it throws an error when the error type column is not a single string", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  expect_error(importerConfiguration$errorTypeColumn <- 1)
  expect_error(importerConfiguration$errorTypeColumn <- c("a", "b"))
  expect_error(importerConfiguration$errorTypeColumn <- NA_character_, "NA")
})

test_that("the error type column is NULL and setting it has no effect without an error column", {
  importerConfiguration <- DataImporterConfiguration$new()
  expect_null(importerConfiguration$errorTypeColumn)
  importerConfiguration$errorTypeColumn <- "Error type"
  expect_null(importerConfiguration$errorTypeColumn)
})

test_that("removing the error column removes the error type column", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$errorTypeColumn <- "Error type"
  importerConfiguration$errorColumn <- NULL
  expect_null(importerConfiguration$errorTypeColumn)
})

test_that("It can print the configuration with an error type column", {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$errorTypeColumn <- "Error type"

  expect_snapshot(print(importerConfiguration))
})

# `ErrorTypeFromColumn.xlsx` has the columns `Id`, `Time [h]`,
# `Concentration [mg/l]`, `Error` and `Error type`, and the sheets:
# - `Mixed`: data set A is geometric (`GSD`, `gsd ` with a trailing space), B has no error type, and
#   C is arithmetic (`Arithmetic Standard Deviation`, `asd`);
# - `Conflict`: data set A has `GSD` and `ASD`;
# - `Unknown`: data set A has the unknown value `SD`.
errorTypeFilePath <- getTestDataFilePath("ErrorTypeFromColumn.xlsx")

errorTypeConfiguration <- function(errorType = DataErrorType$ArithmeticStdDev) {
  importerConfiguration <- DataImporterConfiguration$new()
  importerConfiguration$timeColumn <- "Time [h]"
  importerConfiguration$measurementColumn <- "Concentration [mg/l]"
  importerConfiguration$measurementDimension <- ospDimensions$`Concentration (mass)`
  importerConfiguration$measurementUnit <- "mg/l"
  importerConfiguration$errorColumn <- "Error"
  importerConfiguration$errorUnit <- "mg/l"
  importerConfiguration$errorType <- errorType
  importerConfiguration$errorTypeColumn <- "Error type"
  importerConfiguration$addGroupingColumn("Id")
  importerConfiguration$namingPattern <- "{Id}"
  importerConfiguration
}

# One row per data set with its error type, error unit and error values, sorted
# by data set name. The import stores values in single precision, so they are
# rounded to keep the snapshots readable.
errorSummary <- function(dataSets) {
  dataSets <- dataSets[order(names(dataSets))]
  data.frame(
    name = names(dataSets),
    yErrorType = vapply(dataSets, function(x) x$yErrorType, character(1)),
    yErrorUnit = vapply(dataSets, function(x) x$yErrorUnit, character(1)),
    yErrorValues = vapply(
      dataSets,
      function(x) paste(signif(x$yErrorValues, 6), collapse = ", "),
      character(1)
    ),
    row.names = NULL
  )
}

test_that("loadDataSetsFromExcel reads the error type of each data set from a column", {
  # A is geometric and imported as written, without a unit. B has no value in
  # the column and gets `errorType` (arithmetic). C is arithmetic.
  dataSets <- loadDataSetsFromExcel(
    xlsFilePath = errorTypeFilePath,
    importerConfigurationOrPath = errorTypeConfiguration(),
    sheets = "Mixed"
  )

  expect_snapshot(errorSummary(dataSets))
})

test_that("loadDataSetsFromExcel applies errorType GeometricStdDev to data sets without a value in the error type column", {
  # B has no value in the column and gets `errorType` (geometric). C is
  # arithmetic and keeps the error unit, even with a geometric `errorType`.
  dataSets <- loadDataSetsFromExcel(
    xlsFilePath = errorTypeFilePath,
    importerConfigurationOrPath = errorTypeConfiguration(
      DataErrorType$GeometricStdDev
    ),
    sheets = "Mixed"
  )

  expect_snapshot(errorSummary(dataSets))
})

test_that("the error unit can be set in a configuration created for a file with a geometric error", {
  # The created configuration has a geometric error type and no dimension for
  # the error column. Setting the error unit failed with "attempt to apply
  # non-function".
  importerConfiguration <- createImporterConfigurationForFile(
    errorTypeFilePath,
    sheet = "Mixed"
  )
  expect_equal(importerConfiguration$errorType, DataErrorType$GeometricStdDev)
  expect_error(importerConfiguration$errorUnit <- "foo")
  importerConfiguration$errorUnit <- "mg/l"
  expect_equal(importerConfiguration$errorUnit, "mg/l")

  importerConfiguration$errorTypeColumn <- "Error type"
  importerConfiguration$addGroupingColumn("Id")
  importerConfiguration$namingPattern <- "{Id}"
  dataSets <- loadDataSetsFromExcel(
    xlsFilePath = errorTypeFilePath,
    importerConfigurationOrPath = importerConfiguration
  )
  expect_snapshot(errorSummary(dataSets))
})

test_that("loadDataSetsFromExcel fails for more than one error type in one data set", {
  expect_error(
    loadDataSetsFromExcel(
      xlsFilePath = errorTypeFilePath,
      importerConfigurationOrPath = errorTypeConfiguration(),
      sheets = "Conflict"
    ),
    "errors while parsing",
    fixed = TRUE
  )
})

test_that("loadDataSetsFromExcel restores the sheets of the configuration when the import fails", {
  importerConfiguration <- errorTypeConfiguration()
  importerConfiguration$sheets <- "Mixed"
  expect_error(
    loadDataSetsFromExcel(
      xlsFilePath = errorTypeFilePath,
      importerConfigurationOrPath = importerConfiguration,
      sheets = "Conflict"
    ),
    "errors while parsing",
    fixed = TRUE
  )
  expect_equal(importerConfiguration$sheets, "Mixed")
})

test_that("loadDataSetsFromExcel fails for an unknown error type", {
  expect_error(
    loadDataSetsFromExcel(
      xlsFilePath = errorTypeFilePath,
      importerConfigurationOrPath = errorTypeConfiguration(),
      sheets = "Unknown"
    ),
    "errors while parsing",
    fixed = TRUE
  )
})

test_that("a configuration without error type column loads and imports as before", {
  importerConfiguration <- loadDataImporterConfiguration(
    getTestDataFilePath("ImporterConfiguration.xml")
  )
  expect_equal(importerConfiguration$errorColumn, "Error [%]")
  expect_null(importerConfiguration$errorTypeColumn)

  dataSets <- loadDataSetsFromExcel(
    xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
    importerConfigurationOrPath = importerConfiguration
  )
  expect_gt(length(dataSets), 0)
  expect_equal(
    unique(vapply(dataSets, function(x) x$yErrorType, character(1))),
    DataErrorType$ArithmeticStdDev
  )
})

test_that("the error type column is kept when saving and loading a configuration", {
  filePath <- withr::local_tempfile(fileext = ".xml")
  errorTypeConfiguration(DataErrorType$GeometricStdDev)$saveConfiguration(
    filePath
  )

  expect_true(any(grepl(
    'errorStdDevColumn="Error type"',
    readLines(filePath, warn = FALSE),
    fixed = TRUE
  )))

  importerConfiguration <- loadDataImporterConfiguration(filePath)
  expect_equal(importerConfiguration$errorTypeColumn, "Error type")
  expect_equal(importerConfiguration$errorType, DataErrorType$GeometricStdDev)

  dataSets <- loadDataSetsFromExcel(
    xlsFilePath = errorTypeFilePath,
    importerConfigurationOrPath = importerConfiguration,
    sheets = "Mixed"
  )
  expect_snapshot(errorSummary(dataSets))
})

test_that("a configuration without error type column does not write the attribute", {
  filePath <- withr::local_tempfile(fileext = ".xml")
  importerConfiguration <- errorTypeConfiguration()
  importerConfiguration$errorTypeColumn <- ""
  importerConfiguration$saveConfiguration(filePath)

  expect_false(any(grepl(
    "errorStdDevColumn",
    readLines(filePath, warn = FALSE),
    fixed = TRUE
  )))
})
