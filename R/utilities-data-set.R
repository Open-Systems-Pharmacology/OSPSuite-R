#' Loads data (typically observed data) from a PKML file and creates a `DataSet` from it.
#' The pkml files are typically exported from PK-Sim or MoBi.
#'
#' @param filePath Full path of pkml file containing the observed data to load
#'
#' @examples
#' filePath <- system.file("extdata", "obs_data.pkml", package = "ospsuite")
#'
#' obsData <- loadDataSetFromPKML(filePath)
#' @export
loadDataSetFromPKML <- function(filePath) {
  dataRepository <- .loadDataRepositoryFromPKML(filePath)
  return(DataSet$new(dataRepository = dataRepository))
}

#' Save the `DataSet` to pkml
#' @details Save the `DataSet` to a pkml file that can be loaded by MoBi
#'
#' @param dataSet The `DataSet` object
#' @param filePath Path where the pkml file will be created
#' @export
#'
#' @examples
#' \dontrun{
#' dataSet <- DataSet$new(name = "NewDataSet")
#' dataSet$setValues(xValues = c(1, 2, 3, 4, 5), yValues = c(10, 20, 30, 40, 50))
#' dataSet$saveToPKML(filePath = "../ObsData.pkml")
#' }
saveDataSetToPKML <- function(dataSet, filePath) {
  validateIsString(filePath)
  validateIsOfType(dataSet, "DataSet")
  filePath <- .expandPath(filePath)
  dataRepositoryTask <- .getNetTaskFromCache("DataRepositoryTask")
  dataRepositoryTask$call(
    "SaveDataRepository",
    dataSet$dataRepository,
    filePath
  )
}

#' Converts a list of `DataSet` objects to a data.frame
#'
#' @param dataSets A list of `DataSet` objects or a single `DataSet`
#'
#' @return
#'
#' DataSet objects as data.frame with columns name, xValues, yValues,
#' yErrorValues, xDimension, xUnit, yDimension, yUnit, yErrorType, yErrorUnit,
#' molWeight, lloq, and a column for each meta data that is present in any
#' `DataSet`.
#'
#' @export
dataSetToDataFrame <- function(dataSets) {
  dataSets <- c(dataSets)
  validateIsOfType(dataSets, "DataSet")

  obsData <- data.frame(
    name = .makeDataFrameColumn(dataSets, "name"),
    xValues = .makeDataFrameColumn(dataSets, "xValues"),
    yValues = .makeDataFrameColumn(dataSets, "yValues"),
    yErrorValues = .makeDataFrameColumn(dataSets, "yErrorValues"),
    xDimension = .makeDataFrameColumn(dataSets, "xDimension"),
    xUnit = .makeDataFrameColumn(dataSets, "xUnit"),
    yDimension = .makeDataFrameColumn(dataSets, "yDimension"),
    yUnit = .makeDataFrameColumn(dataSets, "yUnit"),
    yErrorType = .makeDataFrameColumn(dataSets, "yErrorType"),
    yErrorUnit = .makeDataFrameColumn(dataSets, "yErrorUnit"),
    molWeight = .makeDataFrameColumn(dataSets, "molWeight"),
    lloq = .makeDataFrameColumn(dataSets, "LLOQ"),
    stringsAsFactors = FALSE
  )

  # add one column for each metaData that is present in any DataSet
  for (name in unique(unlist(
    lapply(dataSets, \(x) names(x$metaData)),
    use.names = F
  ))) {
    obsData[[name]] <- .makeDataFrameColumn(
      dataSets,
      "metaData",
      metaDataName = name
    )
  }

  # consistently return a (classical) data frame
  return(obsData)
}

#' Create a vector of the right length for a certain property of a `DataSet`
#'
#' @param dataSets A list of `dataSet` objects or a single `dataSet`.
#' @param property The property to create the vector for.
#' @param metaDataName The name of the metaData to create the vector for.
#'
#' @return A vector of length corresponding to dataSet$xValues containing the property values.
#'
#' @keywords internal
.makeDataFrameColumn <- function(dataSets, property, metaDataName = NULL) {
  unlist(
    # unlist to return a vector containing all dataSets data
    lapply(
      dataSets,
      function(dataSet) {
        # check length of entry for a certain property of this data set.
        if (is.null(metaDataName)) {
          len <- length(dataSet[[property]])
        } else {
          len <- length(dataSet[[property]][[metaDataName]])
        }

        if (len == 0) {
          # If property is empty, return NA vector of xValues length
          if (property %in% c("yErrorType", "yErrorUnit")) {
            rep(NA_character_, length(dataSet$xValues))
          } else {
            rep(NA_real_, length(dataSet$xValues))
          }
        } else if (len == 1) {
          # if property is a single value, return value vector of xValues length
          if (is.null(metaDataName)) {
            rep(dataSet[[property]], length(dataSet$xValues))
          } else {
            rep(dataSet[[property]][[metaDataName]], length(dataSet$xValues))
          }
        } else {
          # if property is a vector, return it directly
          dataSet[[property]]
        }
      }
    ),
    use.names = FALSE
  )
}


#' @rdname dataSetToDataFrame
#' @param names Optional character vector of custom names to assign to the datasets.
#'   If provided, must have the same length as the number of DataSet objects.
#'   This allows renaming datasets, which is particularly useful when multiple
#'   datasets have the same original name.
#' @examples
#' # Create datasets with duplicate names
#' ds1 <- DataSet$new(name = "Obs")
#' ds1$setValues(xValues = c(1, 2), yValues = c(10, 20))
#'
#' ds2 <- DataSet$new(name = "Obs")
#' ds2$setValues(xValues = c(3, 4), yValues = c(30, 40))
#'
#' # Convert to tibble with custom names
#' tibble_data <- dataSetToTibble(list(ds1, ds2), names = c("Study1", "Study2"))
#' unique(tibble_data$name) # Returns c("Study1", "Study2")
#'
#' @export
dataSetToTibble <- function(dataSets, names = NULL) {
  # Store the original dataSets before conversion for naming logic
  # Ensure originalDataSets is always a list/vector for consistent handling
  originalDataSets <- c(dataSets)

  obsData <- dataSetToDataFrame(dataSets)

  # Apply custom naming if provided
  if (!is.null(names)) {
    # Get the original dataset names from the input
    # Handle case where dataset might not have a name property
    original_names <- vapply(
      originalDataSets,
      function(ds) {
        if (!is.null(ds$name)) {
          return(ds$name)
        } else {
          return("")
        }
      },
      character(1)
    )

    # Validate names length matches number of datasets
    if (length(names) != length(original_names)) {
      stop(sprintf(
        "Length of 'names' (%d) must match number of datasets (%d)",
        length(names),
        length(original_names)
      ))
    }

    # Vectorized renaming using rep() for better performance
    n_rows_per_dataset <- vapply(
      originalDataSets,
      function(ds) length(ds$xValues),
      integer(1)
    )
    obsData$name <- rep(names, times = n_rows_per_dataset)
  }

  # consistently return a tibble data frame
  return(dplyr::as_tibble(obsData))
}

#' Load data sets from excel
#'
#' @details Load observed data from an excel file using an importer configuration
#'
#' @param xlsFilePath Path to the excel file with the data
#' @param importerConfigurationOrPath An object of type `DataImporterConfiguration` that is valid
#'  for the excel file or a path to a XML file with stored configuration
#' @param importAllSheets If `FALSE` (default), only sheets specified in the
#' `importerConfiguration` will be loaded. If `TRUE`, an attempt to load all sheets
#' is performed. If any sheet does not comply with the configuration, an error is thrown.
#'
#' @return A named set of `DataSet` objects. The naming is defined by the property
#' `importerConfiguration$namingPattern`.
#' @export
#'
#' @examples
#'
#' xlsFilePath <- system.file(
#'   "extdata", "CompiledDataSet.xlsx",
#'   package = "ospsuite"
#' )
#'
#' importerConfiguration <- createImporterConfigurationForFile(xlsFilePath)
#' importerConfiguration$sheets <- "TestSheet_1"
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfigurationOrPath = importerConfiguration,
#'   importAllSheets = FALSE
#' )
#'
#' importerConfigurationFilePath <- system.file(
#'   "extdata", "dataImporterConfiguration.xml",
#'   package = "ospsuite"
#' )
#'
#' dataSets <- loadDataSetsFromExcel(
#'   xlsFilePath = xlsFilePath,
#'   importerConfigurationOrPath = importerConfigurationFilePath,
#'   importAllSheets = FALSE
#' )
loadDataSetsFromExcel <- function(
  xlsFilePath,
  importerConfigurationOrPath,
  importAllSheets = FALSE
) {
  validateIsString(xlsFilePath)
  importerConfiguration <- importerConfigurationOrPath
  if (is.character(importerConfigurationOrPath)) {
    importerConfiguration <- loadDataImporterConfiguration(
      importerConfigurationOrPath
    )
  }
  validateIsOfType(importerConfiguration, "DataImporterConfiguration")
  validateIsLogical(importAllSheets)

  dataImporterTask <- .getNetTaskFromCache("DataImporterTask")
  dataImporterTask$set("IgnoreSheetNamesAtImport", importAllSheets)
  dataRepositories <- dataImporterTask$call(
    "ImportExcelFromConfiguration",
    importerConfiguration,
    xlsFilePath
  )
  dataSets <- lapply(dataRepositories, function(x) {
    repository <- DataRepository$new(x)
    DataSet$new(dataRepository = repository)
  })
  names(dataSets) <- lapply(dataSets, function(x) {
    x$name
  })

  return(dataSets)
}
