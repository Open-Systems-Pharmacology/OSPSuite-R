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
    #' Add simulated data.
    #' @return `DataCombined` object containing simulated data.

    addSimulationResults = function(simulationResults,
                                    groups = NULL,
                                    names = NULL,
                                    quantitiesOrPaths = NULL,
                                    population = NULL,
                                    individualIds = NULL) {
      # fail fast; walk is needed because there is no output here
      purrr::walk(.x = groups, .f = ~ validateIsString(.x, nullAllowed = TRUE))

      # list of `SimulationResults` instances is not allowed
      if (is.list(simulationResults)) {
        stop(
          "Only a single instance, and not a list, of `SimulationResults` objects is expected.",
          call. = FALSE
        )
      }

      # validate the object type
      validateIsOfType(simulationResults, SimulationResults)

      # make sure that length of groups and names is the same
      # additionally since these lists are going to have strings as elements,
      # convert NULLs to NAs
      groups <- .validateListArgs(groups, simulationResults$allQuantityPaths)
      names <- .validateListArgs(names, quantitiesOrPaths %||% simulationResults$allQuantityPaths)

      # save the original object as it is; useful for `$toDataFrame()` method
      # styler: off
      private$.simulationResults              <- simulationResults
      private$.groups$groupsSimulationResults <- groups
      private$.quantitiesOrPaths              <- quantitiesOrPaths
      private$.population                     <- population
      private$.individualIds                  <- individualIds
      # styler: on

      # extract a dataframe and store it internally
      private$.simulationResultsDF <- private$.simResults2DF(
        private$.simulationResults,
        groups,
        names
      )

      # add it to combined dataframe
      private$.dataCombinedDF <- private$.updateDF(private$.dataCombinedDF, private$.simulationResultsDF)

      if (length(purrr::compact(private$.groups)) > 0L) {
        private$.groupMap <- private$.extractGroupMap(private$.dataCombinedDF)
      }

      invisible(self)
    },

    #' @description
    #' Add observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSets = function(dataSets, groups = NULL, names = NULL) {
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

      # if alternate names are provided, change the names
      # for `NULL` elements, the original names will be retained
      if (!is.null(names) && is.list(dataSets)) {
        names <- .validateListArgs(names, names(dataSets))
        names <- ifelse(is.na(names), names(dataSets), names)
      }

      # they should be of same length
      if (!is.null(groups)) {
        validateIsSameLength(dataSets, groups)

        # if a list is specified, convert NULL to NA
        # and then flatten list to a vector
        if (is.list(groups)) {
          groups <- purrr::flatten_chr(purrr::modify(groups, .null_to_na))
        }
      }

      # save the original object as it is; useful for `$toDataFrame()` method
      private$.dataSets <- dataSets

      # save grouping information for observed data
      private$.groups$groupsDataSets <- groups

      # extract a dataframe and store it internally
      private$.dataSetsDF <- private$.dataSet2DF(private$.dataSets, groups, names)

      # add it to combined dataframe
      private$.dataCombinedDF <- private$.updateDF(private$.dataCombinedDF, private$.dataSetsDF)

      if (length(purrr::compact(private$.groups)) > 0L) {
        private$.groupMap <- private$.extractGroupMap(private$.dataCombinedDF)
      }

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

      # styler: off
      private$.names         <- names
      private$.xOffsets      <- xOffsets
      private$.yOffsets      <- yOffsets
      private$.xScaleFactors <- xScaleFactors
      private$.yScaleFactors <- yScaleFactors
      # styler: on

      # transform data
      if (!is.null(private$.dataCombinedDF)) {
        private$.dataCombinedDF <- private$.dataTransform(private$.dataCombinedDF)
      }

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
      if (!is.null(private$.dataCombinedDF)) {
        # consistent column order
        private$.dataCombinedDF <- dplyr::select(
          private$.dataCombinedDF,
          # all identifying columns
          dplyr::matches("^group$"),
          dataType,
          name,
          dplyr::matches("^paths$"),
          dplyr::matches("id$"),
          # everything related to X-variable
          dplyr::matches("^x"),
          # everything related to Y-variable
          dplyr::matches("^y"),
          # everything else goes after that
          dplyr::everything()
        )
      }

      # final dataframe to return
      return(private$.dataCombinedDF)
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

      invisible(self)
    }
  ),

  # active bindings ---------------------------------------------------

  active = list(

    #' @field dataSets Instance (or a `list` of instances) of the `DataSet`
    #'   object(s).

    # don't return `as.list()`, because it just returns a list of public members
    # from the R6 object by calls `as.list.environment()`, which is not
    # particularly useful for the users
    dataSets = function(value) {
      if (missing(value)) {
        private$.dataSets
      } else {
        stop(messages$errorPropertyReadOnly("dataSets"))
      }
    },

    #' @field simulationResults Object of type `SimulationResults` produced by
    #'   calling `runSimulation` on a `Simulation` object.

    # don't run `as.list()` for reasons mentioned above
    simulationResults = function(value) {
      if (missing(value)) {
        private$.simulationResults
      } else {
        stop(messages$errorPropertyReadOnly("simulationResults"))
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
    }
  ),

  # private methods and fields -----------------------------------

  private = list(
    # methods --------------------

    # extract dataframe from DataSet objects
    .dataSet2DF = function(object, groups = NULL, names = NULL) {
      # if a list of DataSet instances, merge iterated dataframes by rows
      if (is.list(object)) {
        data <- purrr::map_dfr(object, dataSetToDataFrame)
      } else {
        data <- dataSetToDataFrame(object)
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

      # add column describing the type of data
      data <- data %>%
        dplyr::mutate(dataType = "observed") %>%
        dplyr::as_tibble()

      data <- private$.addGroupCol(data, groups)

      return(data)
    },

    # extract dataframe from `SimulationResults` objects

    .simResults2DF = function(object, groups = NULL, names = NULL) {
      # all input validation will take place in this function itself
      data <- simulationResultsToDataFrame(
        simulationResults = object,
        quantitiesOrPaths = private$.quantitiesOrPaths,
        population        = private$.population,
        individualIds     = private$.individualIds
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

      # add group column
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
    .updateDF = function(dataOld = NULL, dataNew = NULL) {
      if (!is.null(dataOld)) {
        dataOld <- dplyr::bind_rows(dataOld, dataNew)
      } else {
        dataOld <- dataNew
      }

      dataOld
    },

    # extract dataframe with group mappings
    .extractGroupMap = function(data) {
      data %>%
        dplyr::select(group, name, dataType) %>%
        dplyr::distinct() %>%
        dplyr::arrange(group, name)
    },

    # transform the dataset using specified offsets and scale factors
    .dataTransform = function(data) {
      # select only the selected dataset names and paths
      # if a list of names is provided then the list of other arguments must be
      # checked to be of the same length as this list
      if (!is.null(private$.names)) {
        data <- dplyr::filter(data, name %in% private$.names)

        # offset and scale factor for x- and y-axes
        data <- private$.colTransform(data, private$.xOffsets, xOffsets)
        data <- private$.colTransform(data, private$.xScaleFactors, xScaleFactors)
        data <- private$.colTransform(data, private$.yOffsets, yOffsets)
        data <- private$.colTransform(data, private$.yScaleFactors, yScaleFactors)
      } else {
        data <- dplyr::mutate(
          data,
          xOffsets = private$.xOffsets[[1]],
          yOffsets = private$.yOffsets[[1]],
          xScaleFactors = private$.xScaleFactors[[1]],
          yScaleFactors = private$.yScaleFactors[[1]]
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
        data <- dplyr::mutate(
          data,
          yErrorValues = yErrorValues * yScaleFactors
        )
      }

      # these columns are no longer necessary
      # retaining them might confuse the user about whether the
      # transformations are supposed to be carried out by the user using these
      # values or these transformations have already been carried out by the
      # method using these values
      data <- dplyr::select(data, -dplyr::ends_with(c("Offsets", "ScaleFactors")))

      return(data)
    },

    # to transform a single column
    .colTransform = function(data, arg, colName) {
      if (length(arg) > 1L) {
        validateIsSameLength(arg, private$.names)

        data <- data %>%
          dplyr::group_by(name) %>%
          dplyr::mutate({{ colName }} := arg[match(name, private$.names)][[1]]) %>%
          dplyr::ungroup()
      } else {
        data <- dplyr::mutate(data, {{ colName }} := arg[[1]])
      }

      return(data)
    },

    # fields --------------------

    # styler: off
    .dataSets               = NULL,
    .dataSetsDF             = NULL,
    .simulationResults      = NULL,
    .simulationResultsDF    = NULL,
    .dataCombinedDF         = NULL,
    .groups                 = list(groupsDataSets = NULL, groupsSimulationResults = NULL),
    .groupMap               = NULL,
    .quantitiesOrPaths      = NULL,
    .population             = NULL,
    .individualIds          = NULL,
    .names                  = NULL,
    .xOffsets               = 0,
    .yOffsets               = 0,
    .xScaleFactors          = 1,
    .yScaleFactors          = 1
    # styler: on
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
    validateIsSameLength(expectedLength, arg)

    # if a list is specified, convert NULL to NA
    # and then flatten list to a vector
    if (is.list(arg)) {
      arg <- purrr::flatten_chr(purrr::modify(arg, .null_to_na))
    }
  }

  return(arg)
}
