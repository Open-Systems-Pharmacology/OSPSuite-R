#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storing simulated and/or observed in a single data frame, which
#' can be further used in data wrangling or data visualization pipelines.
#'
#' Additionally, it allows:
#'
#' - Grouping different simulated and/or observed datasets.
#'
#' - Transforming data with given offsets and scale factors.
#'
#' @import tidyr
#' @import ospsuite.utils
#'
#'
#' @param names A string or a `list` of strings assigning new names. These new
#'   names can be either for renaming `DataSet` objects, or for renaming
#'   quantities/paths in `SimulationResults` object. If an entity is not to be
#'   renamed, this can be specified as `NULL`. E.g., in `names = list("oldName1"
#'   = "newName1", "oldName2" = NULL)`), dataset with name `"oldName2"` will not
#'   be renamed. The list can either be named or unnamed. Names act as unique
#'   identifiers for data sets in the `DataCombined` object and, therefore,
#'   duplicate names are not allowed.
#' @param groups A string or a list of strings specifying group name
#'   corresponding to each data set. If an entry within the list is `NULL`, the
#'   corresponding data set is not assigned to any group (and the corresponding
#'   entry in the `group` column will be an `NA`). If provided, `groups` must
#'   have the same length as `dataSets` and/or `simulationResults$quantityPath`.
#'   If no grouping is specified for any of the dataset, the column `group` in
#'   the data frame output will be all `NA`.
#'
#' @examples
#' # simulated data
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simResults <- runSimulations(sim)[[1]]
#' outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
#'
#' # observed data
#' obsData <- lapply(
#'   c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_2.pkml", "ObsDataAciclovir_3.pkml"),
#'   function(x) loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
#' )
#' names(obsData) <- lapply(obsData, function(x) x$name)
#'
#'
#' # Create a new instance of `DataCombined` class
#' myDataCombined <- DataCombined$new()
#'
#' # Add simulated results
#' myDataCombined$addSimulationResults(
#'   simulationResults = simResults,
#'   quantitiesOrPaths = outputPath,
#'   groups = "Aciclovir PVB"
#' )
#'
#' # Add observed data set
#' myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")
#'
#' # Looking at group mappings
#' myDataCombined$groupMap
#'
#' # Looking at the applied transformations
#' myDataCombined$dataTransformations
#'
#' # Accessing the combined data frame
#' myDataCombined$toDataFrame()
#'
#' @family data-combined
#' @docType class
#' @export
DataCombined <- R6::R6Class(
  classname = "DataCombined",
  inherit = Printable,

  # public fields and methods ------------------------------------

  public = list(

    #' @description
    #' Adds observed data.
    #'
    #' @param dataSets An instance (or a `list` of instances) of the `DataSet`
    #'   class.
    #' @param silent A binary flag showing if warnings should be triggered when
    #' data sets are overwritten in the `DataCombined` object
    #'
    #' @return `DataCombined` object containing observed data.
    addDataSets = function(dataSets, names = NULL, groups = NULL, silent = FALSE) {
      # Validate vector arguments' type and length
      validateIsOfType(dataSets, "DataSet", FALSE)
      numberOfDatasets <- objectCount(dataSets)
      names <- .cleanVectorArgs(names, numberOfDatasets, type = "character")

      # The original names for datasets can be "plucked" from objects.
      #
      # `purrr::map()` iterates over the vector and applies the anonymous
      # function to pluck name from the object. The `map_chr()` variant
      # clarifies that we are always expecting a character type in return.
      datasetNames <- purrr::map_chr(c(dataSets), ~ purrr::pluck(.x, "name"))

      # If alternate names are provided for datasets, use them instead.
      #
      # If any of the alternate names are missing, then the original name should
      # be used instead.
      if (!is.null(names) && is.list(dataSets)) {
        names <- ifelse(is.na(names), datasetNames, names)
      }

      # Update private fields and bindings for the new setter call
      private$.dataCombined <- private$.updateDataFrame(
        private$.dataCombined,
        private$.dataSetToDataFrame(dataSets, names),
        silent = silent
      )

      names <- names %||% datasetNames
      # Reset data transformations
      self$setDataTransformations(names)

      private$.updateGroups(names, groups)

      # Set data type
      for (name in names) {
        private$.dataType[[name]] <- "observed"
      }
      # for method chaining
      invisible(self)
    },

    #' @description
    #' Add simulated data using instance of `SimulationResults` class.
    #'
    #' @template simulation_results
    #'
    #' @param silent A binary flag showing if warnings should be triggered when
    #' data sets are overwritten in the `DataCombined` object
    #'
    #' @return `DataCombined` object containing simulated data.
    addSimulationResults = function(simulationResults,
                                    quantitiesOrPaths = NULL,
                                    population = NULL,
                                    individualIds = NULL,
                                    names = NULL,
                                    groups = NULL,
                                    silent = FALSE) {
      # Validate vector arguments' type and length
      validateIsOfType(simulationResults, "SimulationResults", FALSE)

      # A vector of `SimulationResults` class instances is not allowed. Why?
      #
      # If this were to be allowed, `quantitiesOrPaths`, `population`, and
      # `individualIds ` could all be different for every `SimulationResults`
      # instance, and those arguments should also themselves be lists (i.e.,
      # lists of lists), which makes for quite a complicated API.
      if (is.list(simulationResults)) {
        stop(messages$errorOnlyOneSupported())
      }

      # Summary variables (useful since they are referred to more than once)
      pathsNames <- quantitiesOrPaths %||% simulationResults$allQuantityPaths
      pathsLength <- length(pathsNames)

      # Validate alternative names and groups for their length and type
      names <- .cleanVectorArgs(names, pathsLength, type = "character")
      groups <- .cleanVectorArgs(groups, type = "character")

      # If alternate names are provided for datasets, use them instead.
      #
      # If any of the alternate names are missing, then the original name should
      # be used instead.
      if (!is.null(names)) {
        names <- ifelse(is.na(names), pathsNames, names)
      }

      # Update private fields and bindings for the new setter call
      private$.dataCombined <- private$.updateDataFrame(
        dataCurrent = private$.dataCombined,
        dataNew = private$.simResultsToDataFrame(
          simulationResults = simulationResults,
          quantitiesOrPaths = quantitiesOrPaths,
          population        = population,
          individualIds     = individualIds,
          names             = names
        ),
        silent = silent
      )

      names <- names %||% pathsNames
      # Reset data transformations
      self$setDataTransformations(names)

      private$.updateGroups(names, groups)

      # Set data type
      for (name in names) {
        private$.dataType[[name]] <- "simulated"
      }
      # for method chaining
      invisible(self)
    },

    #' @param names A list of dataset names which need to be grouped. Note that
    #'   if you have specified new `names` while adding datasets (using
    #'   `$addDataSets()` and `$addSimulationResults()` methods), you will need
    #'   to use these new names to specify group assignment. The same dataset
    #'   can't be assigned to two different groupings in the *same*
    #'   `$setGroups()` call. In other words, elements of `names` argument
    #'   should be unique.
    #' @param groups A list specifying which datasets belong to which group(s).
    #'   Please note that the order in which groups are specified should match
    #'   the order in which datasets were specified for `names` parameter. For
    #'   example, if data sets are named `"x"`, `"y"`, `"z"`, and the desired
    #'   groupings for them are, respectively, `"a"`, `"b"`, this can be
    #'   specified as `names = list("x", "y"), groups = list("a", "b")`.
    #'   Datasets for which no grouping is to be specified, can be left out of
    #'   the `groups` argument. The column `group` in the data frame output will
    #'   be `NA` for such datasets. If you wish to remove an *existing* grouping
    #'   assignment for a given dataset, you can specify it as following:
    #'   `list("x" = NA)` or `list("x" = NULL)`. This will not change any of the
    #'   other groupings.
    #'
    #' @description
    #' Adds grouping information to (observed and/or simulated) datasets.
    #'
    #' @return `DataCombined` object with grouped datasets.
    setGroups = function(names, groups) {
      # Return early if no datasets are present
      if (is.null(private$.dataCombined)) {
        stop(messages$noDatasetsToGroup())
      }

      # Sanitize vector arguments of `character` type
      names <- .cleanVectorArgs(names, type = "character")
      groups <- .cleanVectorArgs(groups, type = "character")

      # All entered datasets should be unique, name being their identifier.
      validateHasOnlyDistinctValues(names)

      # Inform the user about which datasets specified for grouping are missing
      missingNames <- names[!names %in% self$names]
      if (length(missingNames) > 0) {
        message(messages$printMultipleEntries(
          header  = messages$datasetsToGroupNotFound(),
          entries = missingNames
        ))
      }

      # Update group map data frame
      private$.updateGroups(names, groups)

      # for method chaining
      invisible(self)
    },
    #' @description set the type of data (observed or simulated) for datasets.
    #'
    #' @param names a character vector of dataset names which dataTypes need to
    #' be changed.
    #' @param dataTypes a character vector of dataTypes (`"observed"` or
    #' `"simulated"`) to be assigned to the datasets (in order of `names`.
    #'
    #' @return `DataCombined` object with modified dataTypes datasets.
    setDataTypes = function(names, dataTypes) {
      # Sanitize vector arguments of `character` type
      names <- .cleanVectorArgs(names, type = "character")
      dataTypes <- .cleanVectorArgs(dataTypes, type = "character")

      # Cycle through each name and set the data type
      for (idx in seq_along(names)) {
        name <- names[[idx]]
        dataType <- dataTypes[[idx]]
        if (!dataType %in% c("observed", "simulated")) {
          stop(messages$invalidDataType(name, dataType))
        }

        # Do nothing if the type does not change
        if (dataType == private$.dataType[[name]]) {
          next
        }

        # If the type changes, then the data frame needs to be updated
        private$.dataType[[name]] <- dataType
        private$.dataCombined$dataType[private$.dataCombined$name == name] <- dataType
      }
    },

    #' @param names A list of dataset names whose group assignment needs to be
    #'   removed. Note that if you have specified new `names` while adding
    #'   datasets (using `$addDataSets()` and `$addSimulationResults()`
    #'   methods), you will need to use these new names to specify group
    #'   assignment. The elements of `names` argument should be unique.
    #'
    #' @description
    #' Remove existing groupings for (observed and/or simulated) datasets.
    #'
    #' @return `DataCombined` object with updated group assignments.
    removeGroupAssignment = function(names) {
      for (idx in seq_along(names)) {
        private$.groupMap[[names[[idx]]]] <- NULL
      }

      # for method chaining
      invisible(self)
    },

    #' @description
    #'
    #' Transform raw data with required offsets and scale factors.
    #'
    #' @param forNames A list of names specifying which observed datasets and/or
    #'   paths in simulated dataset to transform with the specified
    #'   transformations. Default is `NULL`, i.e., the transformations, if any
    #'   specified, will be applied to all rows of the data frame.
    #' @param xOffsets,yOffsets,xScaleFactors,yScaleFactors Either a single
    #'   numeric value or a list of numeric values specifying offsets and
    #'   scale factors to apply to raw values. The default offset is `0`, while
    #'   default scale factor is `1`, i.e., the data will not be modified. If a
    #'   list is specified, it should be the same length as `forNames` argument.
    #' @param reset IF `TRUE`, only data transformations that are specified will
    #'   be retained. Not specified transformations will be reset to their defaults.
    #'   Default behavior is `FALSE`, e.g., setting only `xOffsets` will not reset
    #'   `xScaleFactors` if those have been set previously.
    #'
    #' @details
    #'
    #' A data frame with respective raw quantities transformed using specified
    #' offset and scale factor values.
    #'
    #' - For X and Y variables:
    #'   `newValue = (rawValue + offset) * scaleFactor`
    #'
    #' - For error term:
    #'   `newErrorValue = rawErrorValue * scaleFactor`
    setDataTransformations = function(forNames = NULL,
                                      xOffsets = 0,
                                      yOffsets = 0,
                                      xScaleFactors = 1,
                                      yScaleFactors = 1,
                                      reset = FALSE) {
      missingArgs <- list(
        xOffsets = missing(xOffsets),
        yOffsets = missing(yOffsets),
        xScaleFactors = missing(xScaleFactors),
        yScaleFactors = missing(yScaleFactors)
      )
      # Check that the arguments to parameters make sense
      xOffsets <- .cleanVectorArgs(xOffsets, type = "numeric")
      yOffsets <- .cleanVectorArgs(yOffsets, type = "numeric")
      xScaleFactors <- .cleanVectorArgs(xScaleFactors, type = "numeric")
      yScaleFactors <- .cleanVectorArgs(yScaleFactors, type = "numeric")

      # Clean up NAs
      # For offsets: `0` (default for no change)
      xOffsets <- tidyr::replace_na(xOffsets, 0)
      yOffsets <- tidyr::replace_na(yOffsets, 0)
      # For scale factors: `1` (default for no change)
      xScaleFactors <- tidyr::replace_na(xScaleFactors, 1)
      yScaleFactors <- tidyr::replace_na(yScaleFactors, 1)

      forNames <- .cleanVectorArgs(forNames, type = "character")
      # if no names a provided, apply transformations to all data sets.
      if (is.null(forNames)) {
        forNames <- self$names
      }

      # Replace single values by a vector if the same value has to be applied to
      # all names
      if (length(xOffsets) == 1) {
        xOffsets <- rep(xOffsets, length(forNames))
      }

      if (length(yOffsets) == 1) {
        yOffsets <- rep(yOffsets, length(forNames))
      }

      if (length(xScaleFactors) == 1) {
        xScaleFactors <- rep(xScaleFactors, length(forNames))
      }

      if (length(yScaleFactors) == 1) {
        yScaleFactors <- rep(yScaleFactors, length(forNames))
      }
      ospsuite.utils::validateIsSameLength(forNames, xOffsets, yOffsets, xScaleFactors, yScaleFactors)

      # Store values
      for (idx in seq_along(forNames)) {
        name <- forNames[[idx]]
        if (!missingArgs$xOffsets) {
          private$.xOffsets[[name]] <- xOffsets[[idx]]
        } else if (reset) {
          private$.xOffsets[[name]] <- 0
        }

        if (!missingArgs$yOffsets) {
          private$.yOffsets[[name]] <- yOffsets[[idx]]
        } else if (reset) {
          private$.yOffsets[[name]] <- 0
        }

        if (!missingArgs$xScaleFactors) {
          private$.xScaleFactors[[name]] <- xScaleFactors[[idx]]
        } else if (reset) {
          private$.xScaleFactors[[name]] <- 1
        }

        if (!missingArgs$yScaleFactors) {
          private$.yScaleFactors[[name]] <- yScaleFactors[[idx]]
        } else if (reset) {
          private$.yScaleFactors[[name]] <- 1
        }
      }

      # for method chaining
      invisible(self)
    },

    #' @description
    #'
    #' A method to extract a tibble data frame of simulated and/or observed data
    #' (depending on instances of which classes have been added to the object).
    #'
    #' Note that the order in which you enter different object doesn't matter
    #' because the returned data frame is arranged alphabetically by dataset
    #' name.
    #'
    #' @return
    #'
    #' In the returned tibble data frame, the following columns will always be present:
    #'
    #' name - group - dataType - xValues - xDimension - xUnit - yValues -
    #' yErrorValues - yDimension - yUnit - yErrorType - yErrorUnit - molWeight
    #'
    #' @note
    #'
    #' The molecular weight (in `molWeight` column) is in `g/mol` units.
    toDataFrame = function() {
      # quit early if no data is available
      if (is.null(private$.dataCombined)) {
        return(NULL)
      }
      # Add `group` column. Cannot use `mutate` because it would
      # require `rowwise` which kills the performance
      # Fist add empty column
      private$.dataCombined$group <- NA_character_
      for (name in self$names) {
        private$.dataCombined[private$.dataCombined$name == name, ]$group <- private$.groupMap[[name]] %||% NA_character_
      }

      # Apply data transformations
      private$.dataTransform(private$.dataCombined)
    },

    #' @description
    #' Print the object to the console.
    print = function() {
      # Group map contains names, types, and groupings for all datasets, providing
      # the most succinct snapshot of the object.
      private$printClass()
      private$printLine("Datasets and groupings:", addTab = FALSE)
      cat("\n")
      print(self$groupMap)

      # for method chaining
      invisible(self)
    }
  ),

  # active bindings ---------------------------------------------------

  active = list(
    #' @field names A vector of unique names of datasets contained in the
    #'   `DataCombined` class instance.
    names = function(value) {
      if (missing(value)) {
        return(unique(private$.dataCombined$name))
      }

      stop(messages$errorPropertyReadOnly("names"))
    },

    #' @field groupMap A data frame specifying which datasets have been grouped
    #'   together and the name and the nature (observed or simulated?) of the
    #'   data. If a dataset was not assigned to any group, this is denoted by
    #'   `NA` in the data frame.
    groupMap = function(value) {
      if (missing(value)) {
        # quit early if no data is available
        if (is.null(private$.dataCombined)) {
          return(NULL)
        }
        return(
          dplyr::tibble(name = self$names) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              group = private$.groupMap[[name]] %||% NA_character_,
              dataType = private$.dataType[[name]]
            ) %>%
            dplyr::ungroup()
        )
      }

      stop(messages$errorPropertyReadOnly("groupMap"))
    },

    #' @field dataTransformations A data frame with offset and scale factor
    #'   values were specified by the user for each dataset.
    dataTransformations = function(value) {
      if (missing(value)) {
        # For an empty DataCombined, return an etmpy tibble
        if (is.null(private$.dataCombined)) {
          return(
            dplyr::tibble(
              name = character(),
              xOffsets = numeric(),
              yOffsets = numeric(),
              xScaleFactors = numeric(),
              yScaleFactors = numeric()
            )
          )
        }

        return(
          dplyr::tibble(
            name          = self$names,
            # For offsets: `0` (default for no change)
            xOffsets      = 0,
            yOffsets      = 0,
            # For scale factors: `1` (default for no change)
            xScaleFactors = 1,
            yScaleFactors = 1
          ) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              xOffsets = private$.xOffsets[[name]] %||% xOffsets,
              yOffsets = private$.yOffsets[[name]] %||% yOffsets,
              xScaleFactors = private$.xScaleFactors[[name]] %||% xScaleFactors,
              yScaleFactors = private$.yScaleFactors[[name]] %||% yScaleFactors
            ) %>%
            dplyr::ungroup()
        )
      }

      stop(messages$errorPropertyReadOnly("dataTransformations"))
    }
  ),

  # private methods -----------------------------------

  private = list(
    # Extract data frame from `DataSet` object(s)
    .dataSetToDataFrame = function(dataSets, names = NULL) {
      # `dataSetToTibble()` function can extract a tibble data frame from a
      # scalar, a vector, or a list of `DataSet` class instances.
      obsData <- dataSetToTibble(dataSets)

      # Rename, if specific names are provided
      if (!is.null(names)) {
        new_names <- setNames(names, unique(obsData$name))
        obsData$name <- unname(new_names[as.character(obsData$name)])
      }

      # Set type of data
      obsData$dataType <- "observed"

      return(obsData)
    },

    # extract data frame from `SimulationResults` objects
    .simResultsToDataFrame = function(simulationResults,
                                      quantitiesOrPaths = NULL,
                                      population = NULL,
                                      individualIds = NULL,
                                      names = NULL) {
      # `simulationResultsToTibble()` can extract data frame only from a
      # single `SimulationResults` class instance, but this is not a problem
      # because the `$addSimulationResults()` method treats only a single
      # instance as a valid input.
      simData <- simulationResultsToTibble(
        simulationResults = simulationResults,
        quantitiesOrPaths = quantitiesOrPaths,
        population        = population,
        individualIds     = individualIds
      )

      # Simulated datasets and observed datasets are glued row-wise when a
      # combined data frame is prepared, and therefore it is necessary that the
      # same kind of quantities have the same column names so that they are
      # glued appropriately. This requires renaming a few columns.
      # Also rename "paths to "names". If no custom names are specified, the names
      # are always the paths
      simData <- dplyr::rename(simData,
        "xValues" = "Time",
        "xUnit" = "TimeUnit",
        "xDimension" = "TimeDimension",
        "yValues" = "simulationValues",
        "yUnit" = "unit",
        "yDimension" = "dimension",
        "name" = "paths"
      )


      # Update names, if custom names are specified
      if (!is.null(names)) {
        new_names <- setNames(names, unique(simData$name))
        simData <- dplyr::mutate(simData,
          name = new_names[name]
        )
      }

      # Set type of data
      simData$dataType <- "simulated"
      # Simulated results do not have error values
      simData$yErrorValues <- NA_real_

      return(simData)
    },

    # Update the combined data frame "in place"
    .updateDataFrame = function(dataCurrent = NULL, dataNew = NULL, silent = FALSE) {
      # If there is existing data, it will be updated with the new data appended
      # at the bottom.
      if (!is.null(dataCurrent) && !is.null(dataNew)) {
        # The unique identifier for each dataset is its name. Thus, by comparing
        # names, it is checked if the newly entered dataset(s) are already
        # present in the internal combined data frame.
        dupDatasets <- intersect(unique(dataCurrent$name), unique(dataNew$name))

        # If the newly entered dataset(s) are already present, then replace the
        # existing ones with the new ones.
        #
        # For example, someone can all `$addDataSets(dataSet1)` and
        # then again call `$addDataSets(dataSet1)` with the same class
        # instance because they realized that the first time they created the
        # `DataSet` object, they had made a mistake. In this case, data frame
        # created in the latter call should replace the one created in the
        # former call. If we were not to allow this, the user will need to
        # restart with a new instance of this class.
        # Also, a DataFrame can be updated several times by ospsuite. For
        # example, `addDataSet()` calls it twice.
        if (length(dupDatasets) > 0L) {
          # Warn the user if he adds a dataset with a name already used in dataCombined
          if (!silent) {
            messages$DataFrameNameAlreadyUsed(dupDatasets)
          }

          dataCurrent <- dplyr::filter(dataCurrent, !name %in% dupDatasets)
        }

        # Append the new dataset at the bottom of the current one
        dataCurrent <- dplyr::bind_rows(dataCurrent, dataNew)
      } else {
        dataCurrent <- dataNew
      }

      return(dataCurrent)
    },

    # Update group mapping
    .updateGroups = function(names, groups) {
      # If groups is NULL, grouping will be removed for this data set.
      if (!is.null(groups)) {
        # If only one group is provided, it must be assigned to all names
        if (length(groups) == 1) {
          groups <- rep(groups, length(names))
        }
        validateIsSameLength(names, groups)
      }
      # Map groups to names
      for (idx in seq_along(names)) {
        private$.groupMap[[names[[idx]]]] <- groups[[idx]]
      }
    },

    # Transform the dataset using specified offsets and scale factors
    .dataTransform = function(data) {
      # Return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Copy dataTransformations to not alter original object and turn into
      # data.table object
      dataTransformations <- data.table::setDT(data.table::copy(self$dataTransformations))
      # Copy data to not alter original object (private$.dataCombined) and
      # transform into a data.table object
      data <- data.table::setDT(data.table::copy(data))
      # Update values by joining dataTransformations table (base on name column)
      # and apply transformations.
      data <-
        data[dataTransformations,
          `:=`(
            xValues = (xValues + xOffsets) * xScaleFactors,
            yValues = (yValues + yOffsets) * yScaleFactors,
            yErrorValues = yErrorValues * abs(yScaleFactors)
          ),
          on = .(name)
        ] %>%
        # convert back to tibble
        tibble::as_tibble()
      return(data)
    },

    # private fields ----------------------------------------
    .dataCombined = NULL,
    # Mapping of data set name to a group
    .groupMap = list(),
    .dataType = list(),
    .xOffsets = list(),
    .yOffsets = list(),
    .xScaleFactors = list(),
    .yScaleFactors = list()
  ),

  # class default properties ---------------------------------

  lock_objects = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  portable = TRUE
)
