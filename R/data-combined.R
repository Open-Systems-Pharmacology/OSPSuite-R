#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storing simulated and/or observed in a single dataframe, which
#' can be further used for visualization methods.
#'
#' Additionally, it allows:
#'
#' - Grouping different (simulated and/or observed) datasets with grouping
#' labels.
#'
#' - Transforming data (with given offsets and scale factors).
#'
#' @param groups A string or a list of strings assigning the data set to a
#'   group. If an entry within the list is `NULL`, the corresponding data set is
#'   not assigned to any group (and the corresponding entry in the dataframe
#'   will be an `NA`). If provided, `groups` must have the same length as
#'   `dataSets` and/or `simulationResults$quantityPath`. If no grouping is
#'   specified for any of the dataset, the column `group` in the dataframe
#'   output will be all `NA`.
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
#' dataSet <- DataSet$new(name="DS")
#'
#' # created object with datasets combined
#' myCombDat <- DataCombined$new()
#' myCombDat$addSimulationResults(simulationResults)
#' myCombDat$addDataSets(dataSet)
#'
#' # print the object
#' myCombDat
#' @docType class
#' @export

DataCombined <- R6::R6Class(
  classname = "DataCombined",
  inherit = Printable,

  # public fields and methods ------------------------------------

  public = list(

    #' @param dataSets Instance (or a `list` of instances) of the `DataSet`
    #'   class.
    #' @param newNames A string or a list of strings  assigning new names to the
    #'   list of instances of the `DataSet` class. Note that the datasets whose
    #'   names you wish to not change should be specified as `NULL` in the list.
    #'
    #' @description
    #' Adds observed data.
    #'
    #' @return `DataCombined` object containing observed data.
    addDataSets = function(dataSets, newNames = NULL, groups = NULL) {
      # validate vector arguments' type and length
      validateIsOfType(dataSets, "DataSet", FALSE)
      groups <- cleanVectorArgs(groups, objCount(dataSets), type = "character")
      newNames <- cleanVectorArgs(newNames, objCount(dataSets), type = "character")

      # if alternate names are provided for datasets, use them instead
      #
      # for `NULL` elements in a list, the original dataset names will be used
      #
      # to get these names, go inside each element of the list and extract the
      # name (using `purrr::pluck()`) from the object itself and use them
      # instead and simplify to a character vector (using `purrr::map_chr()`)
      if (!is.null(newNames) && is.list(dataSets)) {
        newNames <- ifelse(is.na(newNames), purrr::map_chr(dataSets, ~ purrr::pluck(.x, "name")), newNames)
      }

      # update private fields for the new setter call
      private$.dataCombined <- private$.updateDF(private$.dataCombined, private$.dataSetToDF(dataSets, newNames, groups))
      private$.dataCombined <- private$.extractXYData(private$.dataCombined)
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
      private$.names <- private$.extractNames(private$.dataCombined)

      # set up data transformations
      self$setDataTransformations(newNames)

      # for method chaining
      invisible(self)
    },

    # TODO: if and when this is supported by `{roxygen2}`, inherit parameters
    # from `ospsuite::getOutputValues()` to avoid repetition.

    #' @param simulationResults Object of type `SimulationResults` produced by
    #'   calling `runSimulation` on a `Simulation` object.
    #' @param quantitiesOrPaths Quantity instances (element or list) typically
    #'   retrieved using `getAllQuantitiesMatching` or quantity path (element or
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
    #'   Note that the datasets whose names you wish to not change should be
    #'   specified as `NULL` in the list.
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
                                    newNames = NULL,
                                    groups = NULL) {
      # A list or a vector of `SimulationResults` class instances is not allowed
      #
      # If we were to allow this, `quantitiesOrPaths`, `population`, and
      # `individualIds ` could be different for every `SimulationResult`, and
      # those arguments should also be lists (i.e., lists of lists), which makes
      # for a complicated API.
      #
      # Additionally, if two different `SimulationResults` are added using the
      # same paths, the user **must** provide `names`, otherwise the data will
      # be overwritten (as the default names are the paths).
      #
      # `is.vector()` is not needed since `simResults` are lists, and so both
      # `c(simResults1, simResults2, ...)` and `as.list(simResults1,
      # simResults2, ...)` will return the same result
      if (is.list(simulationResults)) {
        stop(messages$errorOnlyOneSupported())
      }

      # summary variables (created since they are referred to more than once)
      pathsNames <- quantitiesOrPaths %||% simulationResults$allQuantityPaths
      pathsLength <- length(pathsNames)

      # validate vector arguments' type and length
      validateIsOfType(simulationResults, "SimulationResults", FALSE)
      newNames <- cleanVectorArgs(newNames, pathsLength, type = "character")
      groups <- cleanVectorArgs(groups, pathsLength, type = "character")

      # if alternate names are provided for datasets, use them instead
      #
      # for `NULL` elements in a list, which will be converted to `NA`s by
      # `cleanVectorArgs()`, the original dataset names will be used, which
      # are nothing but path names for `SimulationResults` objects (i.e.
      # `simulationResults$allQuantityPaths`)
      if (!is.null(newNames)) {
        newNames <- ifelse(is.na(newNames), pathsNames, newNames)
      }

      # Update private fields for the new setter calls
      private$.dataCombined <- private$.updateDF(
        private$.dataCombined,
        private$.simResultsToDF(
          simulationResults = simulationResults,
          quantitiesOrPaths = quantitiesOrPaths,
          population        = population,
          individualIds     = individualIds,
          names             = newNames,
          groups            = groups
        )
      )
      private$.dataCombined <- private$.extractXYData(private$.dataCombined)
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
      private$.names <- private$.extractNames(private$.dataCombined)

      # set up data transformations
      self$setDataTransformations(newNames)

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
    #'   numeric value or a list of numeric quantities specifying offsets and
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
      # check that the arguments to parameters make sense
      xOffsets <- cleanVectorArgs(xOffsets, type = "numeric")
      yOffsets <- cleanVectorArgs(yOffsets, type = "numeric")
      xScaleFactors <- cleanVectorArgs(xScaleFactors, type = "numeric")
      yScaleFactors <- cleanVectorArgs(yScaleFactors, type = "numeric")
      forNames <- cleanVectorArgs(forNames, type = "character")

      # apply specified data transformations
      private$.dataCombined <- private$.dataTransform(
        data          = private$.dataCombined,
        names         = forNames,
        xOffsets      = xOffsets,
        yOffsets      = yOffsets,
        xScaleFactors = xScaleFactors,
        yScaleFactors = yScaleFactors
      )

      # update private field with transformation values
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
      # It is deliberate that this "cleaning" happens every time the user calls
      # this method. R6 has a reference semantics, i.e., the object will be
      # modified in place and the combined data will be updated with it. But,
      # for future updates, we do need to retain some information that is
      # omitted during cleaning.
      #
      # For example, we remove offset and scale factor columns during cleaning,
      # but they need to be retained in the private copy of the combined
      # dataframe because the datasets that might be added in the future will
      # have their own offsets and scale factors, which will need to be appended
      # to the existing ones.
      private$.cleanDF(private$.dataCombined)
    },

    #' @description
    #' Print the object to the console
    print = function() {
      # group map contains names and nature of the datasets and grouping details
      private$printLine("DataCombined", addTab = FALSE)
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
    .dataSetToDF = function(dataSets, names = NULL, groups = NULL) {
      # `dataSetToDataFrame()` function can handle a vector, a list, or a scalar
      # of `DataSet` class, so use to extract a dataframe, and then
      #
      # add columns with grouping information and additional meta data
      #
      # replace dataset names with alternative names (if provided)
      dataSetToDataFrame(dataSets) %>%
        dplyr::mutate(dataType = "observed") %>%
        private$.renameDatasets(names) %>%
        private$.addGroupCol(groups) %>%
        dplyr::as_tibble()
    },

    # extract dataframe from `SimulationResults` objects
    .simResultsToDF = function(simulationResults,
                               quantitiesOrPaths = NULL,
                               population = NULL,
                               individualIds = NULL,
                               names = NULL,
                               groups = NULL) {
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
          dataType     = "simulated",
          yErrorValues = NA_real_
        ) %>%
        private$.renameDatasets(names) %>%
        private$.addGroupCol(groups) %>%
        dplyr::rename(
          "xValues"    = "Time",
          "xUnit"      = "TimeUnit",
          "xDimension" = "TimeDimension",
          "yValues"    = "simulationValues",
          "yUnit"      = "unit",
          "yDimension" = "dimension"
        )
    },

    # Add a new group column
    #
    # If no grouping is specified for a dataset, the value for `group` column
    # will be `NA`.
    #
    # Looking ahead to the bridge with the `{tlf}` package, while visualizing
    # datasets that don't belong to any grouping, the datset's own name can
    # instead be used as a dummy grouping variable. But this will be taken care
    # of in the plotting function itself.
    #
    # This is why this function doesn't replace `NA`s in the grouping column
    # with dataset names.
    .addGroupCol = function(data, groups = NULL) {
      if (is.null(groups)) {
        return(data %>% dplyr::mutate(group = NA_character_))
      }

      # Nest the dataset by its unique identifier, which is `name`, and then
      # assign new `groups` vector to the new `group` column, and then
      # unnest the list column.
      #
      # Note that `group` column is based on lexical order of quantity paths
      data %>%
        tidyr::nest(data = -name) %>%
        dplyr::mutate(group = groups) %>%
        tidyr::unnest(cols = c(data))
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
    .updateDF = function(dataCurrent = NULL, dataNew = NULL) {
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
      private$.dataTransformations <- private$.updateDF(private$.dataTransformations, dataArg)

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
    .cleanDF = function(data = NULL) {
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
        dplyr::arrange(name)
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
