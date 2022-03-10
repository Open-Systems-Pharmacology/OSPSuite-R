#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storing simulated and/or observed in a single dataframe, which
#' can be further used data wrangling or data visualization pipelines.
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
#' @examples
#'
#' # load the simulation
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simulationResults <- runSimulations(simulations = sim)
#'
#' # create a new dataset object
#' dataSet <- DataSet$new(name = "DS")
#'
#' # created object with datasets combined
#' myCombDat <- DataCombined$new()
#' myCombDat$addSimulationResults(simulationResults)
#' myCombDat$addDataSets(dataSet)
#'
#' # print the object
#' myCombDat
#'
#' @docType class
#'
#' @export
DataCombined <- R6::R6Class(
  classname = "DataCombined",
  inherit = Printable,

  # public fields and methods ------------------------------------

  public = list(

    #' @param dataSets Instance (or a `list` of instances) of the `DataSet`
    #'   class.
    #' @param newNames A string or a list of strings assigning new names to the
    #'   list of instances of the `DataSet` class. If a dataset is not to be
    #'   renamed, this can be specified as `NULL` in the list. For example, in
    #'   `newNames = list("dataName" = "dataNewName", "dataName2" = NULL)`),
    #'   dataset with name `"dataName2"` will not be renamed.
    #'
    #' @description
    #' Adds observed data.
    #'
    #' @return `DataCombined` object containing observed data.
    addDataSets = function(dataSets, newNames = NULL) {
      # Validate vector arguments' type and length
      validateIsOfType(dataSets, "DataSet", FALSE)
      newNames <- cleanVectorArgs(newNames, objCount(dataSets), type = "character")

      # If alternate names are provided for datasets, use them instead.
      #
      # If any of the alternate names are missing, then the original name should
      # be used instead.
      #
      # The original names for datasets can be "plucked" from respective objects.
      if (!is.null(newNames) && is.list(dataSets)) {
        newNames <- ifelse(is.na(newNames), purrr::map_chr(dataSets, ~ purrr::pluck(.x, "name")), newNames)
      }

      # Update private fields and bindings for the new setter call

      private$.dataCombined <- private$.updateDataFrame(
        private$.dataCombined,
        private$.dataSetToDataFrame(dataSets, newNames)
      )

      private$.extractBindings()

      self$setDataTransformations(newNames)

      # for method chaining
      invisible(self)
    },

    # TODO: if and when this is supported by `{roxygen2}`, inherit parameters
    # from `ospsuite::getOutputValues()` to avoid repetition.

    #' @param simulationResults Object of type `SimulationResults` produced by
    #'   calling `runSimulation()` on a `Simulation` object.
    #' @param quantitiesOrPaths Quantity instances (element or list) typically
    #'   retrieved using `getAllQuantitiesMatching()` or quantity path (element or
    #'   list of strings) for which the results are to be returned. (optional)
    #'   When providing the paths, only absolute full paths are supported (i.e.,
    #'   no matching with '*' possible). If `quantitiesOrPaths` is `NULL`
    #'   (default value), returns the results for all output defined in the
    #'   results.
    #' @param individualIds Numeric IDs of individuals for which the results
    #'   should be extracted. By default, all individuals from the results are
    #'   considered. If the individual with the provided ID is not found, the ID
    #'   is ignored.
    #' @param population Population used to calculate the `simulationResults`
    #'   (optional). This is used only to add the population covariates to the
    #'   resulting dataframe.
    #' @param newNames A string or a list of strings assigning new names to the
    #'   quantities or paths present in the entered `SimulationResults` object.
    #'   If a dataset is not to be renamed, this can be specified as `NULL` in
    #'   the list. For example, in `newNames = list("dataName" = "dataNewName",
    #'   "dataName2" = NULL)`), dataset with name `"dataName2"` will not be
    #'   renamed.
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
                                    newNames = NULL) {
      # validate vector arguments' type and length
      validateIsOfType(simulationResults, "SimulationResults", FALSE)

      # A list or a vector of `SimulationResults` class instances is not allowed.
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

      # validate alternative names for their length and type
      newNames <- cleanVectorArgs(newNames, pathsLength, type = "character")

      # If alternate names are provided for datasets, use them instead.
      #
      # If any of the alternate names are missing, then the original name should
      # be used instead.
      if (!is.null(newNames)) {
        newNames <- ifelse(is.na(newNames), pathsNames, newNames)
      }

      # Update private fields and bindings for the new setter call

      private$.dataCombined <- private$.updateDataFrame(
        private$.dataCombined,
        private$.simResultsToDataFrame(
          simulationResults = simulationResults,
          quantitiesOrPaths = quantitiesOrPaths,
          population        = population,
          individualIds     = individualIds,
          names             = newNames
        )
      )

      private$.extractBindings()

      self$setDataTransformations(newNames)

      # for method chaining
      invisible(self)
    },

    #' @param groups A named list specifying which datasets belong to which
    #'   group(s). For example, if datsets are named `"x"`, `"y"`, `"z"`, and
    #'   the desired groupings for them are, respectively, `"a"` and `"b"`, this
    #'   can be specified as `groups = list("x" = "a", "y" = "b")`. Datasets for
    #'   which no grouping is to be specified, can be left out of the `groups`
    #'   argument. The column `group` in the dataframe output will be `NA` for
    #'   such datasets. If you wish to remove existing grouping for a given
    #'   dataset, you can specify it as following: `list("x" = NA)`. This will
    #'   not change any of the other (previously specified) groupings. Note that
    #'   if you have specified `newNames` while adding datasets using respective
    #'   methods, you will need to use these new names to specify group
    #'   assignment.
    #'
    #' @description
    #' Adds grouping information to (observed and/or simulated) datasets.
    #'
    #' @return `DataCombined` object with grouped datasets.
    setGroups = function(groups) {
      if (is.null(private$.dataCombined)) {
        stop("There are currently no datasets to be grouped. You can add them with `$addDataSets()` and/or `$addSimulationResults()` methods.")
      }

      # handle empty lists or lists without names
      if (length(groups) == 0L || is.null(names(groups))) {
        stop("You need to provide a named list with at least one valid grouping.")
      }

      # validate depth of the argument vector
      validateVecDepth(groups)

      # Existing grouping can be removed by setting dataset name to `NA`, but
      # since the default `NA` type in R is `logical`, it needs to be converted
      # to `character` type first.
      groups <- purrr::modify_if(groups, is.na, as.character)

      # If there are any non-`character` type elements in the list, stop
      #
      # we need to check only elements of the list and not the names since names
      # won't ever be anything but of `character` type
      if (length(purrr::keep(groups, ~ !is.character(.))) > 0L) {
        stop("Names for groups can only be of `character` type.")
      }

      # The same dataset can't be assigned to two different groupings in the
      # *same* `$setGroups()` call.
      #
      # This can occur because lists can have multiple elements with the same
      # name.
      if (!hasOnlyDistinctValues(names(groups))) {
        stop("Duplicated dataset names detected. All dataset names must be unique because the same dataset can't be assigned to more than one grouping.")
      }

      # Extract groupings and dataset names in a dataframe.
      #
      # `as.list()` makes sure that both forms of the following specification
      # - `groups = c(...)`
      # - `groups = list(...)`
      # will work.
      groupData <- dplyr::as_tibble(as.list(groups)) %>%
        tidyr::pivot_longer(
          cols      = dplyr::everything(),
          names_to  = "name",
          values_to = "group"
        ) %>%
        # It is important to coerce `group` column to `character` type because
        # if the only column entry is `NA`, it will be of `logical` type, when
        # it should technically be `NA_character_`.
        #
        # Not doing this will cause problems downstream if this column is to be
        # appended to another column that is of `character` type.
        dplyr::mutate(group = as.character(group))

      # Check if any of the specified dataset names are currently not present
      # in the combined dataframe.
      #
      # This can happen when users make spelling mistakes in writing dataset
      # names while specifying groupings, and failing silently will mean that
      # such a mistake will not be brought to the user's attention.
      specifiedNames <- unique(groupData$name)
      currentNames <- unique(private$.dataCombined$name)

      if (!isIncluded(specifiedNames, currentNames)) {
        missingNames <- specifiedNames[!specifiedNames %in% currentNames]

        message(
          cat(
            "Following datasets were specified to be grouped but not found:",
            missingNames,
            sep = "\n"
          )
        )
      }

      # Update the specified groupings with what already exists
      #
      # The object could already have gathered some groupings during its
      # lifetime, and they need to be updated after each `$setGroups()` call.
      groupData <- private$.updateDataFrame(
        dplyr::select(private$.groupMap, -dataType),
        groupData
      )

      # Update grouping information column in the combined dataframe
      private$.dataCombined <- dplyr::left_join(
        x  = dplyr::select(private$.dataCombined, -group),
        y  = groupData,
        by = "name"
      )

      # Update active binding with the new grouping specification
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)

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
    #'   specified, will be applied to all rows of the dataframe.
    #' @param xOffsets,yOffsets,xScaleFactors,yScaleFactors Either a single
    #'   numeric value or a list of numeric values specifying offsets and
    #'   scale factors to apply to raw values. The default offset is `0`, while
    #'   default scale factor is `1`, i.e., the data will not be modified. If a
    #'   list is specified, it should be the same length as `names` argument.
    #'
    #' @details
    #'
    #' A dataframe with respective raw quantities transformed using specified
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
                                      yScaleFactors = 1) {

      # Check that the arguments to parameters make sense
      xOffsets <- cleanVectorArgs(xOffsets, type = "numeric")
      yOffsets <- cleanVectorArgs(yOffsets, type = "numeric")
      xScaleFactors <- cleanVectorArgs(xScaleFactors, type = "numeric")
      yScaleFactors <- cleanVectorArgs(yScaleFactors, type = "numeric")

      forNames <- cleanVectorArgs(forNames, type = "character")

      # Apply specified data transformations
      private$.dataCombined <- private$.dataTransform(
        data          = private$.dataCombined,
        names         = forNames,
        xOffsets      = xOffsets,
        yOffsets      = yOffsets,
        xScaleFactors = xScaleFactors,
        yScaleFactors = yScaleFactors
      )

      # Update private field with transformation values
      private$.dataTransformations <- private$.extractTransforms(private$.dataCombined)

      # for method chaining
      invisible(self)
    },

    #' @description
    #'
    #' A method to extract a dataframe of simulated and/or observed data
    #' (depending on instances of which classes have been added to the object).
    #'
    #' Note that the order in which you enter different object doesn't matter
    #' because the returned dataframe is arranged alphabetically by dataset
    #' name.
    #'
    #' @return
    #'
    #' In the returned dataframe, the following columns will always be present:
    #'
    #' name - group - dataType - xValues - xDimension - xUnit - yValues -
    #' yErrorValues - yDimension - yUnit - yErrorType - yErrorUnit - molWeight
    #'
    #' @note
    #'
    #' The molecular weight (in `molWeight` column) is in `g/mol` units.
    toDataFrame = function() {
      # It is deliberate that the returned dataframe is "cleaned" every time
      # this method is called, instead of just internally storing a copy of
      # cleaned dataframe and returning it.
      #
      # `R6` classes have reference (as opposed to value) semantics. This means
      # the class instance will be modified *in place*, and the combined data
      # will be updated with it for each method call. But, some information
      # omitted during clean-up needs to be retained during the entirety of
      # the object's lifecycle.
      #
      # For example, offset and scale factor columns are removed during
      # cleaning, but they need to be retained in the private copy of the
      # combined dataframe because the datasets that might be added in the
      # future will have their own offsets and scale factors, which will need to
      # be appended to the existing ones.
      private$.cleanDataFrame(private$.dataCombined)
    },

    #' @description
    #' Print the object to the console.
    print = function() {
      # group map contains names and nature of the datasets and grouping details
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

    #' @field groupMap A dataframe specifying which datasets have been grouped
    #'   together and the name and the nature (observed or simulated?) of the
    #'   data. If a dataset was not assigned to any group, this is denoted by
    #'   `NA` in the dataframe.
    groupMap = function(value) {
      if (missing(value)) {
        return(private$.groupMap)
      }

      stop(messages$errorPropertyReadOnly("groupMap"))
    },

    #' @field dataTransformations A dataframe with offset and scale factor
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
    # extract dataframe from `DataSet` objects
    .dataSetToDataFrame = function(dataSets, names = NULL) {
      # `dataSetToDataFrame()` function can handle a vector, a list, or a scalar
      # of `DataSet` class, so use to extract a dataframe, and then
      #
      # add columns with grouping information and additional meta data
      #
      # replace dataset names with alternative names (if provided)
      dataSetToDataFrame(dataSets) %>%
        dplyr::mutate(
          dataType = "observed",
          group    = NA_character_
        ) %>%
        private$.renameDatasets(names) %>%
        dplyr::as_tibble()
    },

    # extract dataframe from `SimulationResults` objects
    .simResultsToDataFrame = function(simulationResults,
                                      quantitiesOrPaths = NULL,
                                      population = NULL,
                                      individualIds = NULL,
                                      names = NULL) {
      # all input validation will take place in this function itself
      # `simulationResultsToDataFrame()` can handle only a single class instance
      # extract a dataframe with is help, and then
      #
      # add a new identifier column with unique names for datasets, which, for
      # these objects, would be `paths` column, and then
      #
      # add column with grouping information, and then
      #
      # add a column describing the type of data;
      # simulation results never have errors, but this leads to inconsistent
      # output, so also add a column for `yErrorValues` outputs, and then
      #
      # rename according to column naming conventions for `DataSet`
      simulationResultsToDataFrame(
        simulationResults = simulationResults,
        quantitiesOrPaths = quantitiesOrPaths,
        population        = population,
        individualIds     = individualIds
      ) %>%
        dplyr::mutate(
          name         = paths,
          group        = NA_character_,
          dataType     = "simulated",
          yErrorValues = NA_real_
        ) %>%
        private$.renameDatasets(names) %>%
        dplyr::rename(
          "xValues"    = "Time",
          "xUnit"      = "TimeUnit",
          "xDimension" = "TimeDimension",
          "yValues"    = "simulationValues",
          "yUnit"      = "unit",
          "yDimension" = "dimension"
        ) %>%
        dplyr::as_tibble()
    },

    # Add a new column with alternate names
    .renameDatasets = function(data, names = NULL) {
      # return early if there is no data
      if (is.null(names)) {
        return(data)
      }

      # Nest the dataset by its unique identifier, which is `name`, and then
      # assign new `names` vector to the existing `name` column, and then
      # unnest the list column.
      #
      # Note that `name` column is based on lexical order of names.
      data %>%
        tidyr::nest(data = -name) %>%
        dplyr::mutate(name = names) %>%
        tidyr::unnest(cols = c(data))
    },

    # update the combined dataframe in place
    .updateDataFrame = function(dataCurrent = NULL, dataNew = NULL) {
      # If there is already data, add new data at the bottom
      #
      # Since the `toDataFrame()` method arranges the data by dataset name,
      # this will not introduce any order effects.
      #
      # For example, it doesn't matter if the user entered `DataSet` objects
      # first and then `SimulationResults` objects, or vice versa
      if (!is.null(dataCurrent) && !is.null(dataNew)) {
        # by comparing names, check if the new dataset(s) entered are already
        # present in the internal combined dataframe
        dupDatasets <- intersect(unique(dataCurrent$name), unique(dataNew$name))

        # if this is the case, then replace the older dataset(s) with the newer
        # version(s) of the same dataset(s)
        #
        # For example, someone can all `$addSimulationResults(dataSet1)` and
        # then again call `$addSimulationResults(dataSet1)` with the same class
        # instance because they realized that the first time they created the
        # DataSet object, they had made a mistake. In this case, dataframe
        # created in the latter call will replace the one created in the former
        # call. If we were not to allow this, the user will need to restart
        # their work with a new instance of this class.
        if (length(dupDatasets) > 0L) {
          dataCurrent <- dplyr::filter(dataCurrent, !name %in% dupDatasets)
        }

        # append the new dataset at the bottom of the current one
        dataCurrent <- dplyr::bind_rows(dataCurrent, dataNew)
      } else {
        dataCurrent <- dataNew
      }

      return(dataCurrent)
    },

    # transform the dataset using specified offsets and scale factors
    .dataTransform = function(data,
                              names = NULL,
                              xOffsets = 0,
                              yOffsets = 0,
                              xScaleFactors = 1,
                              yScaleFactors = 1) {
      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Keep all transformation parameters and their names linked together in a
      # dataframe data structure
      #
      # Additionally, if no names are provides, the transformations will apply
      # to the entire dataframe, and thus dataset names can be a placeholder for
      # the purpose of joining of dataframe with arguments and dataframe with
      # raw data that needs to be transformed
      dataArg <- dplyr::tibble(
        name          = names %||% unique(data$name),
        xOffsets      = xOffsets,
        yOffsets      = yOffsets,
        xScaleFactors = xScaleFactors,
        yScaleFactors = yScaleFactors
      )

      # update dataframe using given transformation parameters
      private$.dataTransformations <- private$.updateDataFrame(private$.dataTransformations, dataArg)

      # if present, remove old parameter columns
      data <- dplyr::select(data, -dplyr::ends_with(c("Offsets", "ScaleFactors")))

      # merge data with corresponding parameters and then
      #
      # replace `NA`s (which will be present for datasets for which no
      # transformations were specified) with default values:
      #
      #  - for offsets: 0
      #  - for scale factors: 1
      #
      # these default values signify that raw data will not be changed
      data <- dplyr::left_join(data, private$.dataTransformations, by = "name") %>%
        dplyr::mutate(across(matches("offsets$"), ~ tidyr::replace_na(.x, 0))) %>%
        dplyr::mutate(across(matches("scalefactors$"), ~ tidyr::replace_na(.x, 1))) %>%
        dplyr::mutate(
          xValues      = (xOriginalValues + xOffsets) * xScaleFactors,
          yValues      = (yOriginalValues + yOffsets) * yScaleFactors,
          yErrorValues = yOriginalErrorValues * yScaleFactors
        )

      return(data)
    },

    # extract all active bindings
    .extractBindings = function() {
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
      private$.dataCombined <- private$.extractXYData(private$.dataCombined)
      private$.names <- private$.extractNames(private$.dataCombined)
    },

    # extract dataframe with group mappings
    .extractGroupMap = function(data) {
      # select only the unique identifier columns and then
      # retain only the distinct combinations and then
      # arrange the dataframe by grouping and names
      data %>%
        dplyr::select(group, name, dataType) %>%
        dplyr::distinct() %>%
        dplyr::arrange(group, name)
    },

    # extract unique and sorted dataset names from the combined dataframe
    .extractNames = function(data = NULL) {
      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      data %>%
        dplyr::pull(name) %>%
        unique() %>%
        sort()
    },

    # Keep internal copies of the raw values before changing them using
    # transformations with given offsets and scale factors.
    #
    # This way, when transformation parameter values are updated, the raw data
    # is still there to be transformed with these new parameters.
    .extractXYData = function(data = NULL) {
      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      data %>%
        dplyr::mutate(
          xOriginalValues      = xValues,
          yOriginalValues      = yValues,
          yOriginalErrorValues = yErrorValues
        )
    },

    # Extract offsets and scale factors used while data transformations
    #
    # Since the user might have entered distinct transformation parameters for
    # each dataset, the combined dataframe needs to be grouped by dataset names.
    .extractTransforms = function(data = NULL) {
      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # select the unique identifier and transformation parameter columns and then
      # group the combined dataframe by unique datasets and then
      # within each dataset retain distinct row combinations and then
      # ungroup the dataframe
      data %>%
        dplyr::select(name, dplyr::matches("offset|scale")) %>%
        dplyr::group_by(name) %>%
        dplyr::distinct() %>%
        dplyr::ungroup()
    },

    # clean dataframe before returning it to the user
    .cleanDataFrame = function(data = NULL) {
      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # having a consistent column order
      data %>%
        dplyr::select(
          # all identifying columns
          name,
          group,
          dataType,
          # everything related to the X-variable
          "xValues", "xUnit", "xDimension", dplyr::matches("^x"),
          # everything related to the Y-variable
          "yValues", "yUnit", "yDimension", dplyr::matches("^y"),
          # all other columns go after that (meta data, etc.)
          dplyr::everything()
        ) %>%
        # the following columns are no longer necessary
        #
        # retaining the offset and scale factor parameters might confuse the
        # user about whether the transformations are supposed to be carried out
        # by the user using these values or these transformations have already
        # been carried out
        #
        # internal copies of raw data and redundant paths columns also need to
        # be left out
        dplyr::select(
          -dplyr::matches("^paths$"),
          -dplyr::matches("offsets$|scalefactors$"),
          # to remove columns containing original data (`xOriginalValues`,
          # `yOriginalValues`, `yOriginalErrorValues`), but retaining other
          # custom metadata users might have specified containing the string
          # `original`
          -(dplyr::contains("original") & dplyr::matches("values$"))
        ) %>%
        # arrange data (alphabetically) by dataset name
        dplyr::arrange(name) %>%
        # always return a tibble
        dplyr::as_tibble()
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
