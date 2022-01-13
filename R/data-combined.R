#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storage of simulated and/or observed data, which can be further
#' used to extract as a dataframe for visualization methods. Additionally, it
#' allows:
#' - Grouping different (simulated and/or observed) datasets with unique
#' grouping labels. If no grouping is specified, name of the dataset is used as
#' a grouping column.
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
    #' The constructor is empty because we prefer that the users add
    #' `SimulationResults` and `DataSet` objects separately and individually.
    #' @return A new instance of `DataCombined` object.

    # empty constructor
    initialize = function() {
    },

    ## setter methods ---------------

    #' @description
    #' Add observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSets = function(dataSets, names = NULL, groups = NULL) {
      # fail fast if this is not correct
      validateIsString(groups, nullAllowed = TRUE)

      # if a list is provided, keep only elements which are of `DataSet` type
      if (is.list(dataSets)) {
        dataSets <- purrr::keep(dataSets, ~ inherits(.x, "DataSet"))

        # if no object of `DataSet` type is retained, inform the user
        if (length(dataSets) == 0L) {
          stop("No `DataSet` object detected.", call. = FALSE)
        }
      } else {
        validateIsOfType(dataSets, DataSet)
      }

      # if alternate namesf or datasets are provided, use them instead
      # for `NULL` elements, the original names will be used
      if (!is.null(names) && is.list(dataSets)) {
        names <- .validateListArgs(names, length(names(dataSets)))
        names <- ifelse(is.na(names), names(dataSets), names)
      }

      # the grouping specification list and the number of datasets should have
      # the same length
      if (!is.null(groups)) {
        validateIsSameLength(dataSets, groups)

        # if a list is specified, convert `NULL` to `NA`
        # and then flatten it to a vector
        if (is.list(groups)) {
          groups <- purrr::flatten_chr(purrr::modify(groups, .null_to_na))
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
      # fail fast; walk is needed because there is no output here
      purrr::walk(.x = groups, .f = ~ validateIsString(.x, nullAllowed = TRUE))

      # list of `SimulationResults` instances is not allowed
      if (is.list(simulationResults)) {
        stop(messages$errorWrongType("simulationResults", "list", "a scalar (vector of length 1)"))
      }

      # validate the object type
      validateIsOfType(simulationResults, SimulationResults)

      # make sure that length of groups and names is the same
      # additionally since these lists are going to have strings as elements,
      # convert NULLs to NAs
      groups <- .validateListArgs(groups, length(simulationResults$allQuantityPaths))
      names <- .validateListArgs(names, length(quantitiesOrPaths %||% simulationResults$allQuantityPaths))

      # save grouping information
      private$.groups$groupsSimulationResults <- groups

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
    #'   multiplied by the specified scale factors.

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
    #' A dataframe of simulated and/or observed data (depending on instances of
    #' which objects have been added to the object).
    #'
    #' Note that the order in which you enter different object matters. If you
    #' first enter observed data and simulated data later, the rows will also be
    #' ordered in the same way.
    #'
    #' @return A dataframe.

    toDataFrame = function() {
      # It is deliberate that this "cleaning" happens every time the user calls
      # this method. R6 has a reference semantics, i.e., the object will be
      # modified in place and the combined data will be updated with it, but we
      # do need to retain some of the information that is omitted during
      # cleaning in the future. For example, we remove offset and scale factor
      # columns while cleaning, but it needs to be retained in the private field
      # because the additional datasets that might be added in the future will
      # have their own parameters that will need to be added
      if (!is.null(private$.dataCombinedDF)) {
        return(private$.cleanDF(private$.dataCombinedDF))
      } else {
        return(NULL)
      }
    },

    ## print method -----------------

    #' @description
    #' Print the object to the console
    #' If `dataSets` is a list of `DataSet` objects, then the print output is
    #' going to be quite long.

    print = function() {
      private$printClass()

      if (!is.null(private$.simulationResults)) {
        private$.simulationResults$print()
      }

      if (!is.null(private$.dataSets)) {
        if (is.list(private$.dataSets)) {
          purrr::walk(private$.dataSets, print)
        } else {
          private$.dataSets$print()
        }
      }

      # for method chaining
      invisible(self)
    }
  ),

  # active bindings ---------------------------------------------------

  active = list(

    #' @field names A vector listing collection of all names of `DataSet`
    #'   objects and/or quantities or paths for `SimulationResuls` object.

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

    #' @field groupMap A dataframe specifying which data sets have been grouped
    #'   together and the name and the nature of the dataset.

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

    #' @field dataTransformations A dataframe specifying which offsets and scale factors values were used to transform data.

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
    # private methods --------------------

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

      # add a column describing the type of data
      data <- data %>%
        dplyr::mutate(dataType = "observed") %>%
        dplyr::as_tibble()

      # add column with grouping information
      data <- private$.addGroupCol(data, groups)

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

      # add column describing the type of data
      data <- dplyr::mutate(data, dataType = "simulated") %>%
        dplyr::as_tibble()

      # rename according to column naming conventions for DataSet
      data <- dplyr::rename(data,
        "xValues"    = "Time",
        "xUnit"      = "TimeUnit",
        "yValues"    = "simulationValues",
        "yUnit"      = "unit",
        "yDimension" = "dimension"
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

      # add column with grouping information
      data <- private$.addGroupCol(data, groups)

      return(data)
    },

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

        # offset and scale factor for x- and y-axes
        data <- data %>%
          private$.colTransform(xOffsets, "xOffsets", names) %>%
          private$.colTransform(xScaleFactors, "xScaleFactors", names) %>%
          private$.colTransform(yOffsets, "yOffsets", names) %>%
          private$.colTransform(yScaleFactors, "yScaleFactors", names)
      } else {
        # otherwise the same values are used for all datasets
        data <- dplyr::mutate(
          data,
          xOffsets = xOffsets,
          yOffsets = yOffsets,
          xScaleFactors = xScaleFactors,
          yScaleFactors = yScaleFactors
        )
      }

      # apply transformations
      data <- dplyr::mutate(
        data,
        xValues = (xValues + xOffsets) * xScaleFactors,
        yValues = (yValues + yOffsets) * yScaleFactors
      )

      # applicable only if the error is available
      if ("yErrorValues" %in% names(data)) {
        data <- dplyr::mutate(data, yErrorValues = yErrorValues * yScaleFactors)
      }

      return(data)
    },

    # to transform a single column
    #
    # Although arguments to parameters `arg` and `colName` are going to be the
    # same, they can't be represented by the same argument since one is
    # providing values while the other is providing a name. Doing so will
    # produce the following error:
    #
    # > promise already under evaluation: recursive default argument reference or
    # > earlier problems?

    .colTransform = function(data, arg, colName, names) {
      if (length(arg) > 1L) {
        validateIsSameLength(arg, names)

        data <- data %>%
          dplyr::group_by(name) %>%
          dplyr::mutate({{ colName }} := arg[match(name, names)][[1]]) %>%
          dplyr::ungroup()
      } else {
        data <- dplyr::mutate(data, {{ colName }} := arg[[1]])
      }

      return(data)
    },

    # extract dataframe with group mappings

    .extractGroupMap = function(data) {
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

    # extract offsets and scale factors used while data transformations

    .extractTransforms = function(data = NULL) {
      data %>%
        dplyr::select(name, dataType, dplyr::matches("offset|scale")) %>%
        dplyr::group_by(name) %>%
        dplyr::distinct() %>%
        dplyr::ungroup()
    },

    # clean dataframe before returning it to the user

    .cleanDF = function(data = NULL) {
      # consistent column order
      data %>%
        dplyr::select(
          # all identifying columns
          dplyr::matches("^group$"),
          dataType,
          name,
          dplyr::matches("id$"),
          # everything related to X-variable
          dplyr::matches("^x"),
          # everything related to Y-variable
          dplyr::matches("^y"),
          # everything else goes after that
          dplyr::everything(),
          # columns to remove
          -dplyr::matches("^paths$")
        ) %>%
        # these columns are no longer necessary
        # retaining them might confuse the user about whether the
        # transformations are supposed to be carried out by the user using these
        # values or these transformations have already been carried out
        dplyr::select(-dplyr::ends_with(c("Offsets", "ScaleFactors")))
    },

    # private fields --------------------

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


# utils ---------------------------------------

#' @title convert `NULL` values to `NA`s
#'
#' @description
#' `NULL`s should be converted to `NA`, because otherwise flattening a list will
#' drop them and the vector lengths will be shorter than argument list lengths
#'
#' @noRd

.null_to_na <- function(x) {
  if (is.null(x)) {
    # since the groups are always going to be strings
    x <- NA_character_
  } else {
    x
  }
}

.validateListArgs <- function(arg = NULL, expectedLength) {
  if (!is.null(arg)) {
    validateIsOfLength(arg, expectedLength)

    # if a list is specified, convert NULL to NA
    # and then flatten list to a vector
    if (is.list(arg)) {
      arg <- purrr::flatten_chr(purrr::modify(arg, .null_to_na))
    }
  }

  return(arg)
}
