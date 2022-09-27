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
#' simResults <- runSimulation(sim)
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

    #' @param dataSets An instance (or a `list` of instances) of the `DataSet`
    #'   class.
    #'
    #' @description
    #' Adds observed data.
    #'
    #' @return `DataCombined` object containing observed data.
    addDataSets = function(dataSets, names = NULL, groups = NULL) {
      # Validate vector arguments' type and length
      validateIsOfType(dataSets, "DataSet", FALSE)
      numberOfDatasets <- objectCount(dataSets)
      names <- .cleanVectorArgs(names, numberOfDatasets, type = "character")

      # The original names for datasets can be "plucked" from objects.
      #
      # `purrr::map()` iterates over the vector and applies the anonymous
      # function to pluck name from the object. The `map_chr()` variant
      # clarifies that we are always expecting a character type in return.
      datasetNames <- purrr::map_chr(c(dataSets), function(x) purrr::pluck(x, "name"))

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
        private$.dataSetToDataFrame(dataSets, names)
      )

      private$.extractBindings()

      self$setDataTransformations(names)

      if (!is.null(groups)) {
        self$setGroups(names %||% datasetNames, groups)
      }

      # for method chaining
      invisible(self)
    },

    # TODO: if and when this is supported by `{roxygen2}`, inherit parameters
    # from `ospsuite::getOutputValues()` to avoid repetition.

    #' @param simulationResults Object of type `SimulationResults` produced by
    #'   calling `runSimulation()` on a `Simulation` object. Only a single
    #'   instance is allowed in a given `$addSimulationResults()` method call.
    #' @param quantitiesOrPaths Quantity instances (element or list) typically
    #'   retrieved using `getAllQuantitiesMatching()` or quantity path (element
    #'   or list of strings) for which the results are to be returned.
    #'   (optional) When providing the paths, only absolute full paths are
    #'   supported (i.e., no matching with '*' possible). If `quantitiesOrPaths`
    #'   is `NULL` (default value), returns the results for all output defined
    #'   in the results.
    #' @param individualIds Numeric IDs of individuals for which the results
    #'   should be extracted. By default, all individuals from the results are
    #'   considered. If the individual with the provided ID is not found, the ID
    #'   is ignored.
    #' @param population Population used to calculate the `simulationResults`
    #'   (optional). This is used only to add the population covariates to the
    #'   resulting data frame.
    #'
    #' @description
    #'
    #' Add simulated data using instance of `SimulationResults` class.
    #'
    #' @return `DataCombined` object containing simulated data.
    addSimulationResults = function(simulationResults,
                                    quantitiesOrPaths = NULL,
                                    population = NULL,
                                    individualIds = NULL,
                                    names = NULL,
                                    groups = NULL) {
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

      # Validate alternative names for their length and type
      names <- .cleanVectorArgs(names, pathsLength, type = "character")

      # If alternate names are provided for datasets, use them instead.
      #
      # If any of the alternate names are missing, then the original name should
      # be used instead.
      if (!is.null(names)) {
        names <- ifelse(is.na(names), pathsNames, names)
      }

      # Update private fields and bindings for the new setter call

      private$.dataCombined <- private$.updateDataFrame(
        private$.dataCombined,
        private$.simResultsToDataFrame(
          simulationResults = simulationResults,
          quantitiesOrPaths = quantitiesOrPaths,
          population        = population,
          individualIds     = individualIds,
          names             = names
        )
      )

      private$.extractBindings()

      self$setDataTransformations(names)

      if (!is.null(groups)) {
        self$setGroups(names %||% pathsNames, groups)
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

      # `names` and `groups` need to be of the same length only if each dataset
      # is assigned to a different group. But it is possible that the users
      # want to assign all entered datasets to the same group.
      #
      # In the latter case, `groups` argument can be a scalar (length 1, i.e.)
      # and we don't need to check that names and groups are of the same length.
      if (length(groups) > 1L) {
        validateIsSameLength(names, groups)
      }

      # All entered datasets should be unique, name being their identifier.
      validateHasOnlyDistinctValues(names)

      # Extract groupings and dataset names in a data frame.
      #
      # `purrr::simplify()` will simplify input vector (which can be an atomic
      # vector or a list) to an atomic vector. That is, it'll cover both of these
      # contexts:
      #
      # - `names/groups = c(...)`
      # - `names/groups = list(...)`
      groupData <- dplyr::tibble(
        name = purrr::simplify(names),
        group = purrr::simplify(groups)
      )

      # Update group map data frame
      private$.updateGroupMap(groupData)

      # for method chaining
      invisible(self)
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
      # Return early if no datasets are present
      if (is.null(private$.dataCombined)) {
        stop(messages$noDatasetsPresentInDataCombined())
      }

      # Sanitize vector arguments of `character` type
      names <- .cleanVectorArgs(names, type = "character")
      validateHasOnlyDistinctValues(names)

      # Extract dataset names in a data frame. Groupings for all of them are
      # going to be `NA`, so make avail of `{tibble}`'s recycling rule.
      groupData <- dplyr::tibble(
        name = purrr::simplify(names),
        group = NA_character_
      )

      # Update group map data frame
      private$.updateGroupMap(groupData)

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
      # Check that the arguments to parameters make sense
      xOffsetsNew <- .cleanVectorArgs(xOffsets, type = "numeric")
      yOffsetsNew <- .cleanVectorArgs(yOffsets, type = "numeric")
      xScaleFactorsNew <- .cleanVectorArgs(xScaleFactors, type = "numeric")
      yScaleFactorsNew <- .cleanVectorArgs(yScaleFactors, type = "numeric")

      forNames <- .cleanVectorArgs(forNames, type = "character")

      # If any of the values is missing, they are retained from already existing values
      if ((!reset) & (!is.null(private$.dataTransformations))) {
        if (any(!missing(xOffsets), !missing(yOffsets), !missing(xScaleFactors), !missing(yScaleFactors))) {
          if (missing(xOffsets)) {
            xOffsetsNew <- private$.dataTransformations$xOffsets
          }
          if (missing(yOffsets)) {
            yOffsetsNew <- private$.dataTransformations$yOffsets
          }
          if (missing(xScaleFactors)) {
            xScaleFactorsNew <- private$.dataTransformations$xScaleFactors
          }
          if (missing(yScaleFactors)) {
            yScaleFactorsNew <- private$.dataTransformations$yScaleFactors
          }
        }
      }


      # Apply specified data transformations
      private$.dataCombined <- private$.dataTransform(
        data          = private$.dataCombined,
        forNames      = forNames,
        xOffsets      = xOffsetsNew,
        yOffsets      = yOffsetsNew,
        xScaleFactors = xScaleFactorsNew,
        yScaleFactors = yScaleFactorsNew
      )

      # Update private field with transformation values
      private$.dataTransformations <- private$.extractTransforms(private$.dataCombined)

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
      # It is deliberate that the returned data frame is "cleaned" every time
      # this method is called, instead of just internally storing a copy of
      # cleaned data frame and returning it.
      #
      # `R6` classes have reference (as opposed to value) semantics. This means
      # the class instance will be modified *in place*, and the combined data
      # will be updated with it for each method call. But, some information
      # omitted during clean-up needs to be retained during the entirety of
      # the object's lifecycle.
      #
      # For example, offset and scale factor columns are removed during
      # cleaning, but they need to be retained in the private copy of the
      # combined data frame because the datasets that might be added in the
      # future will have their own offsets and scale factors, which will need to
      # be appended to the existing ones.
      private$.cleanDataFrame(private$.dataCombined)
    },

    #' @description
    #' Print the object to the console.
    print = function() {
      # Group map contains names, types, and groupings for all datasets, providing
      # the most succinct snapshot of the object.
      private$printClass()
      private$printLine("Datasets and groupings", addTab = FALSE)
      cat("\n")
      print(private$.groupMap)

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
        return(private$.names)
      }

      stop(messages$errorPropertyReadOnly("names"))
    },

    #' @field groupMap A data frame specifying which datasets have been grouped
    #'   together and the name and the nature (observed or simulated?) of the
    #'   data. If a dataset was not assigned to any group, this is denoted by
    #'   `NA` in the data frame.
    groupMap = function(value) {
      if (missing(value)) {
        return(private$.groupMap)
      }

      stop(messages$errorPropertyReadOnly("groupMap"))
    },

    #' @field dataTransformations A data frame with offset and scale factor
    #'   values were specified by the user for each dataset.
    dataTransformations = function(value) {
      if (missing(value)) {
        return(private$.dataTransformations)
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

      # Irrespective of whether groups are specified or not, the data frames
      # always start out with an empty `group` column, which is later modified
      # in `$setGroups()` call.
      obsData <- dplyr::mutate(obsData,
        dataType = "observed",
        group    = NA_character_
      )

      # Use the user-defined new names for datasets
      obsData <- private$.renameDatasets(obsData, names)

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

      # Irrespective of whether groups are specified or not, the data frames
      # always start out with an empty `group` column, which is later modified
      # in `$setGroups()` call.
      simData <- dplyr::mutate(simData,
        name         = paths,
        group        = NA_character_,
        dataType     = "simulated",
        yErrorValues = NA_real_
      )

      # Use the user-defined new names for datasets
      simData <- private$.renameDatasets(simData, names)

      # Simulated datasets and observed datasets are glued row-wise when a
      # combined data frame is prepared, and therefore it is necessary that the
      # same kind of quantities have the same column names so that they are
      # glued appropriately. This requires renaming a few columns.
      simData <- dplyr::rename(simData,
        "xValues"    = "Time",
        "xUnit"      = "TimeUnit",
        "xDimension" = "TimeDimension",
        "yValues"    = "simulationValues",
        "yUnit"      = "unit",
        "yDimension" = "dimension"
      )

      return(simData)
    },

    # Add a new column with alternate names
    .renameDatasets = function(data, names = NULL) {
      # Return early if there is no data
      if (is.null(names)) {
        return(data)
      }

      # Create a nested data frame where all columns except `name` are collapsed
      # into a single column named `data`, which will contain these columns in a
      # list data frame.
      data <- tidyr::nest(data, data = -name)

      # Note that `name` column is based on lexical order of `names`. This
      # always works because the order of data frame (and thus the `name`
      # column) returned by `*ToTibble()` functions never changes.
      data <- dplyr::mutate(data, name = names)

      # Unnest the nested data frame.
      data <- tidyr::unnest(data, cols = c(data))

      return(data)
    },

    # Update the combined data frame "in place"
    .updateDataFrame = function(dataCurrent = NULL, dataNew = NULL) {
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
        if (length(dupDatasets) > 0L) {
          dataCurrent <- dplyr::filter(dataCurrent, !name %in% dupDatasets)
        }

        # Append the new dataset at the bottom of the current one
        dataCurrent <- dplyr::bind_rows(dataCurrent, dataNew)
      } else {
        dataCurrent <- dataNew
      }

      return(dataCurrent)
    },

    # Update group map data frame
    .updateGroupMap = function(groupData) {
      # Check if any of the specified dataset names are currently not present
      # in the combined data frame.
      #
      # This can happen when users make spelling mistakes in writing dataset
      # names while specifying groupings, and failing silently will mean that
      # such a mistake will not be brought to the user's attention.
      specifiedNames <- unique(groupData$name)
      currentNames <- unique(private$.dataCombined$name)

      # Inform the user about which datasets are missing
      if (!isIncluded(specifiedNames, currentNames)) {
        missingNames <- specifiedNames[!specifiedNames %in% currentNames]

        message(messages$printMultipleEntries(
          header = messages$datasetsToGroupNotFound(),
          entries = missingNames
        ))
      }

      # Update the specified groupings with what already exists
      #
      # The object could already have gathered some groupings during its
      # lifetime, and they need to be updated after each `$setGroups()` call.
      groupData <- private$.updateDataFrame(
        dplyr::select(private$.groupMap, -dataType),
        groupData
      )

      # Update grouping information column in the combined data frame
      private$.dataCombined <- dplyr::left_join(
        x  = dplyr::select(private$.dataCombined, -group),
        y  = groupData,
        by = "name"
      )

      # Update active binding with the new grouping specification
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
    },

    # Transform the dataset using specified offsets and scale factors
    .dataTransform = function(data,
                              forNames = NULL,
                              xOffsets = 0,
                              yOffsets = 0,
                              xScaleFactors = 1,
                              yScaleFactors = 1) {
      # Return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Keep all transformation parameters and their names linked together in a
      # data frame data structure.
      #
      # Additionally, if no names are provides, the transformations will apply
      # to the entire data frame, and thus dataset names can be a placeholder for
      # the purpose of joining of data frame with arguments and data frame with
      # raw data that needs to be transformed.
      dataArg <- dplyr::tibble(
        name          = forNames %||% unique(data$name),
        xOffsets      = xOffsets,
        yOffsets      = yOffsets,
        xScaleFactors = xScaleFactors,
        yScaleFactors = yScaleFactors
      )

      # Update data frame using given transformation parameters
      private$.dataTransformations <- private$.updateDataFrame(private$.dataTransformations, dataArg)

      # Every call to method to set transformations refreshes these parameters.
      #
      # Thus, if there are any existing parameters from object's lifecycle,
      # they should be removed.
      data <- dplyr::select(data, -dplyr::ends_with(c("Offsets", "ScaleFactors")))

      # Datasets for which no data transformations were specified, there will be
      # missing values, which need to be replaced by defaults for no change.
      data <- dplyr::left_join(data, private$.dataTransformations, by = "name")

      # For offsets: `0` (default for no change)
      data <- dplyr::mutate(
        data,
        dplyr::across(
          .cols = matches("offsets$"), # relevant only for columns matching this pattern
          .fns = function(x) tidyr::replace_na(x, 0)
        )
      )

      # For scale factors: `1` (default for no change)
      data <- dplyr::mutate(
        data,
        dplyr::across(
          .cols = matches("scalefactors$"), # relevant only for columns matching this pattern
          .fns = function(x) tidyr::replace_na(x, 1)
        )
      )

      # Apply the specified transformations to the columns of interest
      data <- dplyr::mutate(data,
        xValues      = (xRawValues + xOffsets) * xScaleFactors,
        yValues      = (yRawValues + yOffsets) * yScaleFactors,
        yErrorValues = yRawErrorValues * abs(yScaleFactors)
      )

      return(data)
    },

    # Extract all active bindings
    .extractBindings = function() {
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
      private$.dataCombined <- private$.addRawDataColumns(private$.dataCombined)
      private$.names <- private$.extractNames(private$.dataCombined)
    },

    # Extract data frame with group mappings
    .extractGroupMap = function(data) {
      # Retain only the columns that have relevant information for group mapping.
      data <- dplyr::select(data, group, name, dataType)

      # Keep only distinct combinations.
      data <- dplyr::distinct(data)

      # Arrange the dataframe alphabetically:
      # first by group name column and then by dataset name column
      data <- dplyr::arrange(data, group, name)

      return(data)
    },

    # Extract unique and sorted dataset names from the combined data frame
    .extractNames = function(data = NULL) {
      # Return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      return(sort(unique(dplyr::pull(data, name))))
    },

    # During object's lifecycle, the applied data transformations can change,
    # and therefore internal copies of raw data should be retained.
    #
    # This way, when transformation parameter values are updated, the raw data
    # can be re-transformed with the new parameters.
    .addRawDataColumns = function(data = NULL) {
      # Return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Create new columns with internal copies of raw data
      data <- dplyr::mutate(data,
        xRawValues      = xValues,
        yRawValues      = yValues,
        yRawErrorValues = yErrorValues
      )

      return(data)
    },

    # Extract offsets and scale factors used for data transformations for each
    # dataset
    .extractTransforms = function(data = NULL) {
      # Return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Retain only the columns that have relevant information for group mapping.
      data <- dplyr::select(data, name, dplyr::matches("offset|scale"))

      # Keep only distinct combinations *for each dataset (name)*.
      data <- dplyr::group_by(data, name) %>%
        dplyr::distinct() %>%
        dplyr::ungroup()

      return(data)
    },

    # Clean data frame before returning it to the user
    .cleanDataFrame = function(data = NULL) {
      # Return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Returned data frame should always have a consistent column order
      data <- dplyr::select(
        data,
        # All data identifier columns
        name,
        group,
        dataType,
        # Everything related to the X-variable
        "xValues", "xUnit", "xDimension", dplyr::matches("^x"),
        # Everything related to the Y-variable
        "yValues", "yUnit", "yDimension", dplyr::matches("^y"),
        # All other columns go after that (meta data, etc.)
        dplyr::everything()
      )

      # The following columns are no longer necessary
      #
      # Retaining the offset and scale factor parameters might confuse the
      # user about whether the transformations are supposed to be carried out
      # by the user using these values, or these transformations have already
      # been carried out.
      data <- dplyr::select(
        data,
        -dplyr::matches("^paths$"),
        -dplyr::matches("offsets$|scalefactors$"),
        -(dplyr::contains("raw") & dplyr::matches("values$"))
      )

      # Arrange the data frame in alphabetical order of the dataset name.
      data <- dplyr::arrange(data, name)

      # Always a return a tibble data frame
      return(dplyr::as_tibble(data))
    },

    # private fields ----------------------------------------

    .dataCombined = NULL,
    .groupMap = NULL,
    .names = NULL,
    .dataTransformations = NULL
  ),

  # class default properties ---------------------------------

  lock_objects = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  portable = TRUE
)
