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
#' labels. If no grouping is specified, name of the dataset is used as a
#' grouping label.
#'
#' - Transforming data (with given offsets and scale factors).
#'
#' @param simulationResults Object of type `SimulationResults` produced by
#'   calling `runSimulation` on a `Simulation` object.
#' @param quantitiesOrPaths Quantity instances (element or list) typically
#'   retrieved using `getAllQuantitiesMatching` or quantity path (element or
#'   list of strings) for which the results are to be returned. (optional)
#'   When providing the paths, only absolute full paths are supported (i.e., no
#'   matching with '*' possible). If `quantitiesOrPaths` is `NULL` (default
#'   value), returns the results for all output defined in the results.
#' @param individualIds Numeric IDs of individuals for which the results
#'   should be extracted. By default, all individuals from the results are
#'   considered. If the individual with the provided ID is not found, the ID is
#'   ignored.
#' @param population population used to calculate the `simulationResults`
#'   (optional). This is used only to add the population covariates to the
#'   resulting dataframe.
#' @param dataSets Instance (or a `list` of instances) of the `DataSet`
#'   object(s).
#' @param groups A string or a list of strings assigning the data set to a
#'   group. If an entry within the list is `NULL`, the corresponding data set is
#'   not assigned to any group. If `NULL` (default), data sets are not assigned
#'   to any group, instead their name is used as a grouping column. If provided,
#'   `groups` must have the same length as `dataSets`.
#' @param names This argument will be encountered across different methods:
#'
#' - `$setDataTransforms()`: In the context of this method, a list of names
#' specifying which observed datasets and/or paths in simulated dataset to
#' transform. Default is `NULL`, i.e., the transformations will be applied to
#' all rows of the dataframe.
#' - `$addSimulationResults()`: In the context of this method, a list of strings
#' assigning new names to the quantities or paths present in the entered
#' `SimulationResults` object.
#' - `$addDataSets()`: In the context of this method, a list of strings
#' assigning new names to the list of instances of the `DataSet` object.
#'
#' Note that the datasets whose names you wish to not change should be specified
#' as `NULL` in the list.
#' @param xOffsets,yOffsets,xScaleFactors,yScaleFactors Either a numeric scalar
#'   or a list of numeric quantities specifying offsets and scale factors to
#'   apply to raw values. The default offset is `0`, while default scale factor
#'   is `1`, i.e., the data will not be modified. If a list is specified, it
#'   should be the same length as `names` argument.
#'
#' @import tidyr
#' @importFrom rlang ":=" "%||%"
#' @importFrom ospsuite.utils Printable
#' @importFrom ospsuite.utils enum enumGetValue enumHasKey enumKeys enumPut enumRemove enumValues
#' @importFrom ospsuite.utils formatNumerics
#' @importFrom ospsuite.utils getEnumKey
#' @importFrom ospsuite.utils hasUniqueValues
#' @importFrom ospsuite.utils ifNotNull
#' @importFrom ospsuite.utils isFileExtension isIncluded isOfLength isOfType isSameLength
#' @importFrom ospsuite.utils toList
#' @importFrom ospsuite.utils validateEnumValue
#' @importFrom ospsuite.utils validateIsIncluded
#' @importFrom ospsuite.utils validateIsInteger
#' @importFrom ospsuite.utils validateIsLogical
#' @importFrom ospsuite.utils validateIsNumeric
#' @importFrom ospsuite.utils validateIsOfLength
#' @importFrom ospsuite.utils validateIsOfType
#' @importFrom ospsuite.utils validateIsSameLength
#' @importFrom ospsuite.utils validateIsString
#' @importFrom ospsuite.utils validatePathIsAbsolute
#'
#' @examples
#'
#' # load the simulation
#' simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
#' sim <- loadSimulation(simFilePath)
#' simulationResults <- runSimulations(simulations = sim)
#'
#' # create a new dataset object
#' dataSet <- DataSet$new()
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

    ## constructor method ---------------

    #' @description
    #' Initialize a new instance of the class.
    #'
    #' The constructor is empty because we prefer that the users add `DataSet`
    #' and `SimulationResults` objects separately and individually using the
    #' dedicated methods.
    #'
    #' @return A new instance of `DataCombined` object.

    # empty constructor
    initialize = function() {
    },

    ## setter methods ---------------

    #' @description
    #' Adds observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSets = function(dataSets, names = NULL, groups = NULL) {
      # fail fast if datatypes are incorrect
      # purrr::walk is needed below because there is no output here

      # groups are strings or characters
      purrr::walk(.x = groups, .f = ~ validateIsString(.x, nullAllowed = TRUE))

      # if a list is provided, check each element is a `DataSet` instance
      # `NULL` elements are not accepted in argument list here
      if (is.list(dataSets)) {
        purrr::walk(.x = dataSets, .f = ~ validateIsOfType(.x, "DataSet", nullAllowed = FALSE))
      } else {
        # if a single instance is entered instead, validate it
        validateIsOfType(dataSets, DataSet, nullAllowed = FALSE)
      }

      # if alternate names are provided for datasets, use them instead
      # for `NULL` elements, the original names will be used
      if (!is.null(names) && is.list(dataSets)) {
        # anticipate that although a list of `DataSet` objects might be entered,
        # they might not have names associated with them in the container list
        # in such cases, go inside each element of the list (purrr::map) and
        # extract (purrr::pluck) the name from the object itself and use them
        # instead and simplify to a character vector
        if (is.null(names(dataSets))) {
          names(dataSets) <- purrr::map_chr(dataSets, .f = ~ purrr::pluck(.x, "name"))
        }

        # lengths of alternate names and objects should be same
        names <- private$.validateListArgs(names, length(names(dataSets)))

        # if any of the alternate names `NULL`, use original names
        names <- ifelse(is.na(names), names(dataSets), names)
      }

      # length of list with grouping specification and the number of datasets
      # should be same
      if (!is.null(groups)) {
        validateIsOfLength(groups, private$.objCount(dataSets))

        # if a list is specified, convert `NULL` to `NA` and then flatten it to a vector
        if (is.list(groups)) {
          groups <- purrr::flatten_chr(purrr::modify(groups, private$.null_to_na))
        }
      }

      # save grouping information for observed data
      private$.groups$groupsDataSets <- groups

      # extract dataframe and append it to the combined dataframe
      private$.dataCombinedDF <- private$.updateDF(
        private$.dataCombinedDF,
        private$.dataSet2DF(dataSets, names, groups)
      )

      # update group map
      if (length(purrr::compact(private$.groups)) > 0L) {
        private$.groupMap <- private$.extractGroupMap(private$.dataCombinedDF)
      }

      # update dataset names
      private$.names <- private$.extractNames(private$.dataCombinedDF)

      # for method chaining
      invisible(self)
    },

    #' @description
    #' Add simulated data.
    #' @return `DataCombined` object containing simulated data.

    addSimulationResults = function(simulationResults,
                                    quantitiesOrPaths = NULL,
                                    population = NULL,
                                    individualIds = NULL,
                                    names = NULL,
                                    groups = NULL) {
      # fail fast if datatypes are incorrect
      # purrr::walk is needed below because there is no output here
      purrr::walk(.x = groups, .f = ~ validateIsString(.x, nullAllowed = TRUE))

      # list of `SimulationResults` instances is not allowed
      if (is.list(simulationResults)) {
        stop(messages$errorWrongType("simulationResults", "list", "a scalar (vector of length 1)"))
      }

      # validate the object type
      # `NULL` elements are not accepted here
      validateIsOfType(simulationResults, SimulationResults)

      # make sure that length of groups and names is the same
      # additionally since these lists are going to have strings as elements,
      # convert `NULL`s to `NA`s
      groups <- private$.validateListArgs(groups, length(simulationResults$allQuantityPaths))
      names <- private$.validateListArgs(names, length(quantitiesOrPaths %||% simulationResults$allQuantityPaths))

      # extract dataframe and append it to the combined dataframe
      private$.dataCombinedDF <- private$.updateDF(
        private$.dataCombinedDF,
        private$.simResults2DF(
          simulationResults = simulationResults,
          quantitiesOrPaths = quantitiesOrPaths,
          population        = population,
          individualIds     = individualIds,
          names             = names,
          groups            = groups
        )
      )

      # save grouping information
      private$.groups$groupsSimulationResults <- groups

      # update group map
      # group map can only be generated if at least one grouping is specified
      if (length(purrr::compact(private$.groups)) > 0L) {
        private$.groupMap <- private$.extractGroupMap(private$.dataCombinedDF)
      }

      # update dataset names
      private$.names <- private$.extractNames(private$.dataCombinedDF)

      # for method chaining
      invisible(self)
    },

    #' @description
    #' Transform raw data with required offsets and scale factors.
    #' @return A dataframe with respective raw quantities plus offsets
    #'   multiplied by the specified scale factors. If error column is present,
    #'   it will also be scaled.

    setDataTransforms = function(names = NULL,
                                 xOffsets = 0,
                                 yOffsets = 0,
                                 xScaleFactors = 1,
                                 yScaleFactors = 1) {
      # check that the arguments to parameters make sense
      validateIsString(names, nullAllowed = TRUE)
      validateIsNumeric(xOffsets)
      validateIsNumeric(yOffsets)
      validateIsNumeric(xScaleFactors)
      validateIsNumeric(yScaleFactors)

      # transform data
      if (!is.null(private$.dataCombinedDF)) {
        private$.dataCombinedDF <- private$.dataTransform(
          data          = private$.dataCombinedDF,
          names         = names,
          xOffsets      = xOffsets,
          yOffsets      = yOffsets,
          xScaleFactors = xScaleFactors,
          yScaleFactors = yScaleFactors
        )

        # extract a dataframe with data transformations values
        private$.dataTransformations <- private$.extractTransforms(private$.dataCombinedDF)
      }

      # for method chaining
      invisible(self)
    },

    ## getter methods ---------------

    #' @description
    #' A method to extract a dataframe of simulated and/or observed data
    #' (depending on instances of which objects have been added to the object).
    #'
    #' Note that the order in which you enter different object matters. If you
    #' first enter observed data and simulated data later, the rows will also be
    #' in the same order in the returned dataframe.
    #'
    #' @return
    #'
    #' A dataframe.
    #'
    #' The length (number of columns) and number of rows of a dataframe will
    #' change depending on instances of which objects and how many objects have
    #' been provided to the object. During the object's lifetime, this method
    #' will return an updated version of a dataframe that combines observed and
    #' simulated data contained in provided objects.
    #'
    #'   For the same reasons, the dataframe might contain some or all of the
    #'   following columns:
    #'
    #'     group
    #'     dataType
    #'     name
    #'     Group Id
    #'     xValues
    #'     xDimension
    #'     xUnit
    #'     yValues
    #'     yErrorValues
    #'     yDimension
    #'     yUnit
    #'     yErrorType
    #'     yErrorUnit
    #'     molWeight
    #'     lloq
    #'     Source
    #'     Sheet
    #'     Organ
    #'     Compartment
    #'     Molecule


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
      # to the existing ones to be returned by the `dataTransformations()`
      # active binding. If we were to clean the dataframe in place, this will
      # not be possible since there won't be offset and scale factors columns to
      # append to.
      if (!is.null(private$.dataCombinedDF)) {
        return(private$.cleanDF(private$.dataCombinedDF))
      } else {
        return(NULL)
      }
    },

    ## print method -----------------

    #' @description
    #' Print the object to the console

    print = function() {
      private$printClass()

      # TODO: what do we want to print here?!

      # for method chaining
      invisible(self)
    }
  ),

  # active bindings ---------------------------------------------------

  active = list(

    #' @field names A vector of unique names of `DataSet` objects and/or
    #'   quantities or paths from `SimulationResuls` object.

    # just a way to access whatever was specified
    names = function(value) {
      if (missing(value)) {
        private$.names
      } else {
        stop(messages$errorPropertyReadOnly(
          "names",
          optionalMessage = "Names are assigned using `names` argument in `$addSimulationResults()` or `$addDataSets()` methods."
        ))
      }
    },

    #' @field groupMap A dataframe specifying which datasets have been grouped
    #'   together and the name and the nature (observed or simulated?) of the data.

    # just a way to access whatever was specified
    groupMap = function(value) {
      if (missing(value)) {
        private$.groupMap
      } else {
        stop(messages$errorPropertyReadOnly(
          "groupMap",
          optionalMessage = "Data sets are assigned to groups when adding via `$addSimulationResults()` or `$addDataSets()` methods."
        ))
      }
    },

    #' @field dataTransformations A dataframe specifying which offsets and scale
    #'   factor values were specified by the user for each dataset.

    # just a way to access whatever was specified
    dataTransformations = function(value) {
      if (missing(value)) {
        private$.dataTransformations
      } else {
        stop(messages$errorPropertyReadOnly("dataTransformations"))
      }
    }
  ),

  # private -----------------------------------

  private = list(
    ## private methods --------------------

    ## dataframe extractors ---------------------

    # extract dataframe from DataSet objects
    .dataSet2DF = function(dataSets, names = NULL, groups = NULL) {
      # if a list of DataSet instances, merge iterated dataframes by rows
      if (is.list(dataSets)) {
        data <- purrr::map_dfr(dataSets, dataSetToDataFrame)
      } else {
        data <- dataSetToDataFrame(dataSets)
      }

      # if alternative names are provided, use them
      if (!is.null(names)) {
        data <- data %>%
          dplyr::group_by(name) %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(name = names) %>%
          tidyr::unnest(cols = c(data))
      }

      # add column with grouping information and then
      # add a column describing the type of data and then
      # convert to tibble before returning the dataframe
      data <- data %>%
        private$.addGroupCol(groups) %>%
        dplyr::mutate(dataType = "observed") %>%
        dplyr::as_tibble()

      return(data)
    },

    # extract dataframe from `SimulationResults` objects

    .simResults2DF = function(simulationResults,
                              quantitiesOrPaths = NULL,
                              population = NULL,
                              individualIds = NULL,
                              names = NULL,
                              groups = NULL) {
      # all input validation will take place in this function itself
      data <- simulationResultsToDataFrame(
        simulationResults = simulationResults,
        quantitiesOrPaths = quantitiesOrPaths,
        population        = population,
        individualIds     = individualIds
      )

      # if alternative names are provided, use them
      if (!is.null(names)) {
        data <- data %>%
          dplyr::group_by(paths) %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(name = names) %>%
          tidyr::unnest(cols = c(data)) %>%
          dplyr::mutate(
            group = dplyr::case_when(
              is.na(name) ~ paths,
              TRUE ~ name
            )
          )
      }

      # if names are not specified, use paths as unique names
      if (!"name" %in% names(data)) {
        data <- dplyr::mutate(data, name = paths)
      }

      # add column with grouping information and then
      # add a column describing the type of data and then
      # rename according to column naming conventions for `DataSet` and then
      # convert to tibble before returning the dataframe
      data <- data %>%
        private$.addGroupCol(groups) %>%
        dplyr::mutate(dataType = "simulated") %>%
        dplyr::rename(
          "xValues"    = "Time",
          "xUnit"      = "TimeUnit",
          "yValues"    = "simulationValues",
          "yUnit"      = "unit",
          "yDimension" = "dimension"
        ) %>%
        dplyr::as_tibble()

      return(data)
    },

    ## dataframe modifiers ---------------------

    # add a new group column
    # if no grouping is specified, this is going to be same as name

    .addGroupCol = function(data, groups = NULL) {
      if (!is.null(groups)) {
        data <- data %>%
          dplyr::group_by(name) %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(group = groups) %>%
          tidyr::unnest(cols = c(data)) %>%
          dplyr::mutate(
            group = dplyr::case_when(
              is.na(group) ~ name,
              TRUE ~ group
            )
          )
      } else {
        data <- data %>% dplyr::mutate(group = name)
      }

      return(data)
    },

    # update the combined dataframe in place

    .updateDF = function(dataCurrent = NULL, dataNew = NULL) {
      # if there is already data, add new data at the bottom
      # note that this introduces order effects, i.e.,
      # whether `DataSet` is added `SimulationResults` or vice versa matters
      # for how the corresponding data rows are ordered in the dataframe
      if (!is.null(dataCurrent) && !is.null(dataNew)) {
        # check if there are duplicated datasets between these two dataframes
        dupDatasets <- intersect(unique(dataCurrent$name), unique(dataNew$name))

        # if there are duplicated datasets, then remove the older ones
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

      # if a list of names is provided then the list of other arguments must be
      # checked to be of the same length as this list and a separate
      # transformation values needs to be stored for each dataset
      if (!is.null(names)) {
        # filter out datasets with selected names
        data <- dplyr::filter(data, name %in% names)
      } else {
        # if names are not provided then the entire dataframe will be used
        names <- unique(data %>% dplyr::pull(name))
      }

      # apply offset and scale factor for x- and y-axes
      #
      # The private `$.colTransform()`method internally takes care of applying
      # transformation to each dataset separately if a list of parameters is
      # provided, or use the same parameters for the entire dataframe otherwise
      data <- data %>%
        private$.colTransform(xOffsets, "xOffsets", names) %>%
        private$.colTransform(xScaleFactors, "xScaleFactors", names) %>%
        private$.colTransform(yOffsets, "yOffsets", names) %>%
        private$.colTransform(yScaleFactors, "yScaleFactors", names)

      # apply transformations
      data <- dplyr::mutate(
        data,
        xValues = (xValues + xOffsets) * xScaleFactors,
        yValues = (yValues + yOffsets) * yScaleFactors
      )

      # applicable only if the error values are available
      if ("yErrorValues" %in% names(data)) {
        data <- dplyr::mutate(data, yErrorValues = yErrorValues * yScaleFactors)
      }

      return(data)
    },

    # To transform a single column
    #
    # Although arguments to parameters `arg` and `colName` are going to be the
    # same, they can't be represented by the same parameter in method signature
    # since one is providing values while the other is providing a name. Doing
    # so will produce the following error:
    #
    # > promise already under evaluation: recursive default argument reference or
    # > earlier problems?

    .colTransform = function(data, arg, colName, names = NULL) {
      # if a list is provided
      if (length(arg) > 1L) {
        validateIsSameLength(arg, names)

        data <- data %>%
          dplyr::group_by(name) %>%
          dplyr::mutate({{ colName }} := arg[match(name, names)][[1]]) %>%
          dplyr::ungroup()
      } else {
        # if a scalar is provided
        data <- dplyr::mutate(data, {{ colName }} := arg[[1]])
      }

      return(data)
    },

    ## extractor for active bindings ---------------------

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

    # extract unique dataset names from the combined dataframe

    .extractNames = function(data = NULL) {
      if (!is.null(data)) {
        unique(data %>% dplyr::pull(name))
      } else {
        NULL
      }
    },

    # Extract offsets and scale factors used while data transformations
    #
    # Since the user might have entered distinct transformation parameters for
    # each dataset, the combined dataframe needs to be grouped by dataset names.

    .extractTransforms = function(data = NULL) {
      # select the unique identifier and transformation parameter columns and then
      # group the combined dataframe by unique datasets and then
      # within each dataset retain distinct row combinations and then
      # ungroup the dataframe
      data <- data %>%
        dplyr::select(name, dataType, dplyr::matches("offset|scale")) %>%
        dplyr::group_by(name) %>%
        dplyr::distinct() %>%
        dplyr::ungroup()

      return(data)
    },

    # clean dataframe before returning it to the user

    .cleanDF = function(data = NULL) {
      # having a consistent column order
      data %>%
        dplyr::select(
          # all identifying columns
          dplyr::matches("^group$"),
          dataType,
          name,
          dplyr::matches("id$"),
          # everything related to the X-variable
          dplyr::matches("^x"),
          # everything related to the Y-variable
          dplyr::matches("^y"),
          # all other columns go after that
          dplyr::everything(),
          # columns to remove (using -)
          -dplyr::matches("^paths$")
        ) %>%
        # the following columns are no longer necessary
        # retaining them might confuse the user about whether the
        # transformations are supposed to be carried out by the user using these
        # values or these transformations have already been carried out
        dplyr::select(-dplyr::ends_with(c("Offsets", "ScaleFactors")))
    },

    ## utilities ---------------------

    # convert `NULL` values to `NA`s
    #
    # `NULL`s should be converted to `NA`, because otherwise flattening a list
    # will drop them and the resulting vectors will be shorter than argument lists

    .null_to_na = function(x) {
      if (is.null(x)) {
        # since the groups are always going to be strings
        x <- NA_character_
      } else {
        x
      }
    },

    # Extract how many objects were entered, which will be used to decide if
    # the lengths of `names`, `groups`, etc. arguments are acceptable.
    #
    # for example,
    # - if `dataSet` is given a list of 5 `DataSet` objects, then 5
    # - if `dataSet` is given a single `DataSet` object, then 1
    # - `SimulationResults` is always going to be a single object, then 1
    #
    # Note that `length()` won't work here as it will return the number of named
    # objects in the `DataSet` or `SimulationResults` environments, which
    # is not what we want

    .objCount = function(x) {
      if (is.list(x)) {
        l <- length(x)
      } else {
        l <- length(list(x))
      }

      return(l)
    },

    # Serves two purposes while validating arguments that are entered as lists:
    #
    # - Arguments like `names`, `groups`, etc. need to be of the same length
    # as the entered datasets. This is checked here.
    #
    # - We allow entering `NULL` for elements where the users don't expect any
    # change. But, if a list with `NULL` is flattened to a vector, it will be
    # dropped and the length of list of arguments will become shorter. This is
    # taken care of using the private `.null_to_na()` method.

    .validateListArgs = function(arg = NULL, expectedLength) {
      if (!is.null(arg)) {
        validateIsOfLength(arg, expectedLength)

        # if a list is specified,
        # modify it in place by converting `NULL` to `NA` and then
        # flatten the list to an atomic vector of character type
        if (is.list(arg)) {
          arg <- arg %>%
            purrr::modify(private$.null_to_na) %>%
            purrr::flatten_chr(.)
        }
      }

      return(arg)
    },

    ## private fields --------------------

    .dataCombinedDF = NULL,
    .groups = list(groupsDataSets = NULL, groupsSimulationResults = NULL),
    .groupMap = NULL,
    .names = NULL,
    .dataTransformations = NULL
  ),

  # other object properties --------------------------------------

  lock_objects = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  portable = TRUE
)
