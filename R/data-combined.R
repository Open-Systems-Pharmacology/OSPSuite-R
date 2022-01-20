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
#' @param population Population used to calculate the `simulationResults`
#'   (optional). This is used only to add the population covariates to the
#'   resulting dataframe.
#' @param dataSets Instance (or a `list` of instances) of the `DataSet`
#'   class.
#' @param groups A string or a list of strings assigning the data set to a
#'   group. If an entry within the list is `NULL`, the corresponding data set is
#'   not assigned to any group (and the corresponding entry in the dataframe
#'   will be an `NA`). If provided, `groups` must have the same length as
#'   `dataSets` and/or `simulationResults`. If no grouping is specified for any
#'   of the dataset, the column `group` in the dataframe output will be all
#'   `NA`.
#' @param names A string or a list of string. This argument will be encountered
#'   across different methods:
#' - `$setDataTransformations()`: In the context of this method, a list of names
#' specifying which observed datasets and/or paths in simulated dataset to
#' transform with the specified transformations. Default is `NULL`, i.e., the
#' transformations, if any specified, will be applied to all rows of the
#' dataframe.
#' - `$addSimulationResults()`: In the context of this method, a list of strings
#' assigning new names to the quantities or paths present in the entered
#' `SimulationResults` object.
#' - `$addDataSets()`: In the context of this method, a list of strings
#' assigning new names to the list of instances of the `DataSet` class.
#'
#' Note that the datasets whose names you wish to not change should be specified
#' as `NULL` in the list.
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
    #' @return A new (empty) instance of `DataCombined` class.

    # empty constructor
    initialize = function() {
    },

    ## setter methods ---------------

    #' @description
    #' Adds observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSets = function(dataSets, names = NULL, groups = NULL) {
      # validate the object type (`NULL` elements are not accepted)
      # if list of instances is provided, `purrr::walk` will check each element
      if (is.list(dataSets)) {
        purrr::walk(.x = dataSets, .f = ~ validateIsOfType(.x, "DataSet", nullAllowed = FALSE))
      } else {
        validateIsOfType(dataSets, "DataSet", nullAllowed = FALSE)
      }

      # validate argument type and length
      groups <- validateVectorArgs(groups, objCount(dataSets), type = "character")

      # anticipate that although a list of `DataSet` objects might be entered,
      # they might not have names associated with them in the container list
      # in such cases, go inside each element of the list (purrr::map) and
      # extract (purrr::pluck) the name from the object itself and use them
      # instead and simplify to a character vector
      #
      # since we don't allow entering a list of `SimulationResults` objects, we
      # don't need to run a similar check in the relevant method
      if (is.list(dataSets) && is.null(names(dataSets))) {
        names(dataSets) <- purrr::map_chr(dataSets, .f = ~ purrr::pluck(.x, "name"))
      }

      # if alternate names are provided for datasets, use them instead
      # for `NULL` elements, the original names will be used
      if (!is.null(names) && is.list(dataSets)) {
        # lengths of alternate names and objects should be same
        names <- validateVectorArgs(names, length(names(dataSets)), type = "character")

        # if any of the alternate names `NULL`, use original names
        names <- ifelse(is.na(names), names(dataSets), names)
      }

      # update private fields

      # extract dataframe and append it to the combined dataframe
      private$.dataCombined <- private$.updateDF(
        private$.dataCombined,
        private$.dataSetToDF(dataSets, names, groups)
      )

      # update group map
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)

      # update dataset names
      private$.names <- private$.extractNames(private$.dataCombined)

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
      # list or a vector of `SimulationResults` class instances is not allowed
      if (is.vector(simulationResults)) {
        stop(messages$errorOnlyOneSupported())
      }

      # validate the object type (`NULL` elements are not accepted)
      validateIsOfType(simulationResults, SimulationResults, nullAllowed = FALSE)

      # validate argument type and length
      groups <- validateVectorArgs(groups, length(quantitiesOrPaths %||% simulationResults$allQuantityPaths), type = "character")
      names <- validateVectorArgs(names, length(quantitiesOrPaths %||% simulationResults$allQuantityPaths), type = "character")

      # update private fields

      # extract dataframe and append it to the combined dataframe
      private$.dataCombined <- private$.updateDF(
        private$.dataCombined,
        private$.simResultsToDF(
          simulationResults = simulationResults,
          quantitiesOrPaths = quantitiesOrPaths,
          population        = population,
          individualIds     = individualIds,
          names             = names,
          groups            = groups
        )
      )

      # group map can only be generated if at least one grouping is specified
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)

      # update dataset names
      private$.names <- private$.extractNames(private$.dataCombined)

      # for method chaining
      invisible(self)
    },

    #' @description
    #'
    #' Transform raw data with required offsets and scale factors.
    #'
    #' @param xOffsets,yOffsets,xScaleFactors,yScaleFactors Either a numeric
    #'   scalar or a list of numeric quantities specifying offsets and scale
    #'   factors to apply to raw values. The default offset is `0`, while
    #'   default scale factor is `1`, i.e., the data will not be modified. If a
    #'   list is specified, it should be the same length as `names` argument.
    #'
    #' @return A dataframe with respective raw quantities plus offsets
    #'   multiplied by the specified scale factors. If error column is present,
    #'   it will also be scaled.

    setDataTransformations = function(names = NULL,
                                      xOffsets = 0,
                                      yOffsets = 0,
                                      xScaleFactors = 1,
                                      yScaleFactors = 1) {
      # check that the arguments to parameters make sense
      xOffsets <- validateVectorArgs(xOffsets, type = "numeric")
      yOffsets <- validateVectorArgs(yOffsets, type = "numeric")
      xScaleFactors <- validateVectorArgs(xScaleFactors, type = "numeric")
      yScaleFactors <- validateVectorArgs(yScaleFactors, type = "numeric")
      names <- validateVectorArgs(names, type = "character")

      # transform data
      private$.dataCombined <- private$.dataTransform(
        data          = private$.dataCombined,
        names         = names,
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

    ## getter methods ---------------

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
    #' A dataframe with some or all of the following columns:
    #'
    #'     name
    #'     group
    #'     dataType
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
    #'     Group Id
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
      private$.cleanDF(private$.dataCombined)
    },

    ## print method -----------------

    #' @description
    #' Print the object to the console

    print = function() {
      # group map contains names of the included datasets and grouping details
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

    #' @field names A vector of unique names of `DataSet` objects and/or
    #'   quantities or paths from `SimulationResuls` object sorted
    #'   alphabetically.

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
    #'   together and the name and the nature (observed or simulated?) of the
    #'   data. If a dataset was not assigned to any group, this is denoted by
    #'   `NA` in the dataframe.

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
    .dataSetToDF = function(dataSets, names = NULL, groups = NULL) {
      # the dataframe function handles a vector, a list, or a single instance of
      # `DataSet` class
      data <- dataSetToDataFrame(dataSets)

      # if alternative names are provided, use them
      data <- private$.renameDatasets(data, names)

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

    .simResultsToDF = function(simulationResults,
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

      # if names are not present, use paths as unique names
      if (!"name" %in% names(data)) {
        data <- dplyr::mutate(data, name = paths)
      }

      # if alternative names are provided, use them
      if (!is.null(names)) {
        data <- private$.renameDatasets(data, names) %>%
          dplyr::mutate(
            name = dplyr::case_when(
              is.na(name) ~ paths,
              TRUE ~ name
            )
          )
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

    # Add a new group column
    #
    # If no grouping is specified for a dataset, the group will be `NA`.
    #
    # While visualizing such data, the datset name can instead be used as a
    # grouping variable, but this association will happen in the plotting
    # function itself, and not in the output produced by the current object.

    .addGroupCol = function(data, groups = NULL) {
      if (!is.null(groups)) {
        data <- data %>%
          dplyr::group_by(name) %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(group = groups) %>%
          tidyr::unnest(cols = c(data))
      } else {
        data <- data %>% dplyr::mutate(group = NA_character_)
      }

      return(data)
    },

    # update the combined dataframe in place

    .updateDF = function(dataCurrent = NULL, dataNew = NULL) {
      # if there is already data, add new data at the bottom
      # since the `toDataFrame()` method arranges the data by dataset name,
      # this will not introduce any order effects, i.e., it doesn't matter
      # if the user entered `DataSet` objects first and then `SimulationResults`
      # objects, or vice versa
      if (!is.null(dataCurrent) && !is.null(dataNew)) {
        # check for duplicated datasets between the current and the new dataframes
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

      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # Keep all transformation parameters and their names linked together with
      # dataframe data structure
      #
      # In case a list of parameters is specified, `flattenList()` will flatten
      # it to an atomic vector of double type
      #
      # Additionally, if no names are provides, the transformations will apply
      # to the entire dataframe, and thus dataset names can be placeholder just
      # for the purpose of joining two dataframes
      dataArg <- dplyr::tibble(
        name          = names %||% unique(data$name),
        xOffsets      = xOffsets,
        yOffsets      = yOffsets,
        xScaleFactors = xScaleFactors,
        yScaleFactors = yScaleFactors
      )

      # update dataframe with transformation parameters
      private$.dataTransformations <- private$.updateDF(private$.dataTransformations, dataArg)

      # if present, remove old parameter columns
      data <- dplyr::select(data, -dplyr::ends_with(c("Offsets", "ScaleFactors")))

      # merge data with corresponding parameters and then
      # replace `NA`s (which will be present for datasets for which no
      # transformations were specified) with default values for offsets (0) and
      # scale factors (1)
      # these defaults mean no change is made to the data
      data <- dplyr::left_join(data, private$.dataTransformations, by = "name") %>%
        dplyr::mutate(across(matches("offsets$"), ~ tidyr::replace_na(.x, 0))) %>%
        dplyr::mutate(across(matches("scalefactors$"), ~ tidyr::replace_na(.x, 1)))

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

    # add a new column with alternate names
    #
    # first group the dataset by its unique identifier, which is name, and then
    # nest the rest of the dataframe in a list column, and then
    # ungroup the dataframe as mutating column is not going to be by group, and then
    # assign new `names` vector to the existing `name` column, and then
    # unnest the list column

    .renameDatasets = function(data, names = NULL) {
      if (!is.null(names)) {
        data <- data %>%
          dplyr::group_by(name) %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(name = names) %>%
          tidyr::unnest(cols = c(data))
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

    # extract unique and sorted dataset names from the combined dataframe

    .extractNames = function(data = NULL) {
      if (!is.null(data)) {
        data %>%
          dplyr::pull(name) %>%
          unique() %>%
          sort()
      } else {
        NULL
      }
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
      data <- data %>%
        dplyr::select(name, dplyr::matches("offset|scale")) %>%
        dplyr::group_by(name) %>%
        dplyr::distinct() %>%
        dplyr::ungroup()

      return(data)
    },

    # clean dataframe before returning it to the user

    .cleanDF = function(data = NULL) {
      # fail early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      # having a consistent column order
      data <- data %>%
        dplyr::select(
          # all identifying columns
          name,
          group,
          dataType,
          # everything related to the X-variable
          dplyr::matches("^x"),
          # everything related to the Y-variable
          dplyr::matches("^y"),
          # all other columns go after that (meta data, etc.)
          dplyr::everything(),
          # columns to remove (using -)
          -dplyr::matches("^paths$")
        ) %>%
        # the following columns are no longer necessary
        # retaining them might confuse the user about whether the
        # transformations are supposed to be carried out by the user using these
        # values or these transformations have already been carried out
        dplyr::select(-dplyr::ends_with(c("Offsets", "ScaleFactors"))) %>%
        # arrange data (alphabetically) by dataset name
        dplyr::arrange(name)

      # return the cleaned dataframe
      return(data)
    },

    ## private fields --------------------

    .dataCombined = NULL,
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

# utilities ---------------------

# TODO: move these functions to the utilities package

# Convert `NULL` or `NA` to `NA` of desired type
#
# `NULL`s should be converted to `NA`, because otherwise flattening a list
# will drop them and the resulting vectors will be shorter than argument
# lists
#
# Additionally, if users instead enter a vector with character types and
# `NA`s, the default `NA` in R is of logical type, and therefore needs
# to be explicitly cast to `NA_character_`.

#' @keywords internal

toMissingOfType <- function(x, type) {
  # everything other than value will be converted to `NA` of desire type
  if (is.null(x) || is.na(x) || is.nan(x) || is.infinite(x)) {
    x <- switch(type,
      "character" = NA_character_,
      "numeric" = ,
      "real" = ,
      "double" = NA_real_,
      "integer" = NA_integer_,
      "complex" = NA_complex_,
      "logical" = NA,
      stop("Incorrect type entered.")
    )
  }

  return(x)
}

# Extract how many objects were entered, which will be used to decide if
# the lengths of `names`, `groups`, etc. arguments are acceptable.
#
# for example,
# - if `dataSet` is a list of 5 `DataSet` class instances, then 5
# - if `dataSet` is a single `DataSet` class instance, then 1
# - `SimulationResults` is allowed to be only a single instance, then 1
#
# Note that `length()` won't work here. It will return the number of named
# objects in the `DataSet` or `SimulationResults` environments, which is not
# what we want. For example, if `dataSet` is given a single `DataSet`
# object, then the length will be 22, and not 1.

#' @keywords internal

objCount <- function(x) {
  # `is.vector()` can handle both atomic vectors and lists, i.e.
  # both `is.vector(c(1, 2))` and `is.vector(list(1, 2))` will be `TRUE`
  if (is.vector(x)) {
    l <- length(x)
  } else {
    l <- length(list(x))
  }

  return(l)
}


# Serves following purposes while validating arguments entered as lists:
#
# - Arguments like `names`, `groups`, etc. need to be of the same length
# as the entered datasets. This is checked here.
#
# - Additionally, the function checks if the object is of the correct type.
#
# - We allow entering `NULL` for elements where the users don't expect any
# change. But, if a list with `NULL` is flattened to a vector, it will be
# dropped and the length of list of arguments will become shorter. This is
# taken care of using the `toMissingOfType()` function.

#' @keywords internal

validateVectorArgs <- function(arg = NULL, expectedLength = NULL, type) {
  # return early if NULL
  if (is.null(arg)) {
    return(NULL)
  }

  # validate the length of vector arguments
  if (!is.null(expectedLength)) {
    validateIsOfLength(arg, expectedLength)
  }

  # convert `NULL`s or logical `NA`s to `NA` of required type
  # `purrr::map()` will return a list
  arg <- purrr::map(arg, ~ toMissingOfType(.x, type))

  # validate the type of arguments
  # `nullAllowed = TRUE` is necessary because `NULL` in vector arguments is
  # used to specify no change for the corresponding dataset
  # `purrr::walk()` is needed below because validation helper functions
  # are called only for their side effects
  purrr::walk(.x = arg, .f = ~ validateIsOfType(.x, type, nullAllowed = TRUE))

  # arguments are still in a list
  # flatten them to an atomic vector of required type
  arg <- flattenList(arg, type)

  return(arg)
}

#'
#' @keywords internal

flattenList <- function(x, type) {
  if (is.list(x)) {
    x <- switch(type,
      "character" = purrr::flatten_chr(x),
      "numeric" = ,
      "real" = ,
      "double" = purrr::flatten_dbl(x),
      "integer" = purrr::flatten_int(x),
      "logical" = purrr::flatten_lgl(x),
      purrr::flatten(x)
    )
  }

  return(x)
}
