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
#'   `dataSets` and/or `simulationResults`. If no grouping is specified for any
#'   of the dataset, the column `group` in the dataframe output will be all
#'   `NA`.
#' @param names A string or a list of string. This argument will be encountered
#'   across different methods:
#'
#' - `$setDataTransformations()`: In the context of this method, a list of names
#' specifying which observed datasets and/or paths in simulated dataset to
#' transform with the specified transformations. Default is `NULL`, i.e., the
#' transformations, if any specified, will be applied to all rows of the
#' dataframe.
#'
#' - `$addSimulationResults()`: In the context of this method, a list of strings
#' assigning new names to the quantities or paths present in the entered
#' `SimulationResults` object. Note that the datasets whose names you wish to
#' not change should be specified as `NULL` in the list.
#'
#' - `$addDataSets()`: In the context of this method, a list of strings
#' assigning new names to the list of instances of the `DataSet` class. Note
#' that the datasets whose names you wish to not change should be specified as
#' `NULL` in the list.
#'
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

    ## setter methods ---------------

    #' @param dataSets Instance (or a `list` of instances) of the `DataSet`
    #'   class.
    #'
    #' @description
    #' Adds observed data.
    #'
    #' @return `DataCombined` object containing observed data.
    addDataSets = function(dataSets, names = NULL, groups = NULL) {
      # validate argument types and lengths

      # if list of instances is provided, `purrr::walk()` will check each element
      # `purrr::walk()` will in no way modify objects provided to it
      if (is.list(dataSets)) {
        purrr::walk(.x = dataSets, .f = ~ validateIsOfType(.x, "DataSet", FALSE))
      } else {
        validateIsOfType(dataSets, "DataSet", FALSE)
      }

      # anticipate that although a list of `DataSet` objects might be entered,
      # they might not have names associated with them in the argument list
      #
      # in such cases, go inside each element of the list (`purrr::map_chr()`)
      # and extract (`purrr::pluck()`) the name from the object itself and use
      # them instead and simplify to a character vector
      if (is.list(dataSets) && is.null(names(dataSets))) {
        names(dataSets) <- purrr::map_chr(dataSets, .f = ~ purrr::pluck(.x, "name"))
      }

      # if alternate names are provided for datasets, use them instead
      # for `NULL` elements in a list, the original names will be used
      if (!is.null(names) && is.list(dataSets)) {
        # lengths of alternate names and objects should be same
        names <- validateVectorArgs(names, length(names(dataSets)), type = "character")

        # if any of the alternate names `NULL`, use original names
        names <- ifelse(is.na(names), names(dataSets), names)
      }

      # validate argument type and length
      groups <- validateVectorArgs(groups, objCount(dataSets), type = "character")

      # Update private fields for the new setter call

      private$.dataCombined <- private$.updateDF(private$.dataCombined, private$.dataSetToDF(dataSets, names, groups))
      private$.dataCombined <- private$.extractXYData(private$.dataCombined)
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
      private$.names <- private$.extractNames(private$.dataCombined)

      # set up data transformations
      self$setDataTransformations(names)

      # for method chaining
      invisible(self)
    },

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
      # list or a vector of `SimulationResults` class instances is not allowed
      # `is.vector()` will cover both `c(simResults1, simResults2, ...)` and
      # `list(simResults1, simResults2, ...)` possibilities
      if (is.vector(simulationResults)) {
        stop(messages$errorOnlyOneSupported())
      }

      # validate argument types and lengths
      validateIsOfType(simulationResults, SimulationResults, FALSE)
      lengthPaths <- length(quantitiesOrPaths %||% simulationResults$allQuantityPaths)
      names <- validateVectorArgs(names, lengthPaths, type = "character")
      groups <- validateVectorArgs(groups, lengthPaths, type = "character")

      # Update private fields for the new setter call

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
      private$.dataCombined <- private$.extractXYData(private$.dataCombined)
      private$.groupMap <- private$.extractGroupMap(private$.dataCombined)
      private$.names <- private$.extractNames(private$.dataCombined)

      # set up data transformations
      self$setDataTransformations(names)

      # for method chaining
      invisible(self)
    },

    #' @description
    #'
    #' Transform raw data with required offsets and scale factors.
    #'
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

      # apply specified data transformations
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

    #' @field names A vector of unique names of datasets contained in the
    #'   `DataCombined` class instance.
    names = function(value) {
      if (missing(value)) {
        return(private$.names)
      }

      stop(messages$errorPropertyReadOnly(
        "names",
        optionalMessage = "Names are assigned using `names` argument in `$addSimulationResults()` or `$addDataSets()` methods."
      ))
    },

    #' @field groupMap A dataframe specifying which datasets have been grouped
    #'   together and the name and the nature (observed or simulated?) of the
    #'   data. If a dataset was not assigned to any group, this is denoted by
    #'   `NA` in the dataframe.

    groupMap = function(value) {
      if (missing(value)) {
        return(private$.groupMap)
      }

      stop(messages$errorPropertyReadOnly(
        "groupMap",
        optionalMessage = "Data sets are assigned to groups when adding via `$addSimulationResults()` or `$addDataSets()` methods."
      ))
    },

    #' @field dataTransformations A dataframe specifying which offsets and scale
    #'   factor values were specified by the user for each dataset.

    dataTransformations = function(value) {
      if (missing(value)) {
        return(private$.dataTransformations)
      }

      stop(messages$errorPropertyReadOnly("dataTransformations"))
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

      # if `name` column is not present, use `paths` column as unique names
      if (!"name" %in% names(data)) {
        data <- dplyr::mutate(data, name = paths)
      }

      # if alternative names are provided, replace current names with them
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
      #
      # add a column describing the type of data;
      # simulation results never have errors, but this leads to inconsistent
      # output, so also add a column for `yErrorValues` outputs and then
      #
      # rename according to column naming conventions for `DataSet` and then
      data <- data %>%
        private$.addGroupCol(groups) %>%
        dplyr::mutate(
          dataType     = "simulated",
          yErrorValues = NA_real_
        ) %>%
        dplyr::rename(
          "xValues"    = "Time",
          "xUnit"      = "TimeUnit",
          "xDimension" = "TimeDimension",
          "yValues"    = "simulationValues",
          "yUnit"      = "unit",
          "yDimension" = "dimension"
        )

      return(data)
    },

    ## dataframe modifiers ---------------------

    # Add a new group column
    #
    # If no grouping is specified for a dataset, the group will be `NA`.
    #
    # Looking ahead at the bridge with the {tlf} package, while visualizing
    # datasets that don't belong to any grouping, the datset's own name can
    # instead be used as a dummy grouping. But this will be taken care of in the
    # plotting function itself. This is why this function doesn't replace `NA`s
    # in the grouping column with dataset names.

    .addGroupCol = function(data, groups = NULL) {
      if (is.null(groups)) {
        data <- data %>% dplyr::mutate(group = NA_character_)
      } else {
        data <- data %>%
          tidyr::nest(data = !name) %>%
          dplyr::mutate(group = groups) %>%
          tidyr::unnest(cols = c(data))
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
        # check if the new dataset(s) entered are already present in the
        # internal combined dataframe
        dupDatasets <- intersect(unique(dataCurrent$name), unique(dataNew$name))

        # if this is the case, then replace the older datasets with the newer
        # versions of the same datasets
        #
        # e.g. someone can all `$addSimulationResults(dataSet1)` and then again
        # call `$addSimulationResults(dataSet1)` with the same class instance.
        # In this case, dataframe created in the latter call will replace the
        # one created in the former call
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
      # replace `NA`s (which will be present for datasets for which no
      # transformations were specified) with default values:
      #
      #  - for offsets: 0
      #  - for scale factors: 1
      #
      # these default values mean raw data will not be changed
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

    # add a new column with alternate names
    #
    # first group the dataset by its unique identifier, which is name, and then
    # nest the rest of the dataframe in a list column, and then
    # ungroup the dataframe as mutating column is not going to be by group, and then
    # assign new `names` vector to the existing `name` column, and then
    # unnest the list column

    .renameDatasets = function(data, names = NULL) {
      # return early if there is no data
      if (is.null(names)) {
        return(data)
      }

      data %>%
        tidyr::nest(data = !name) %>%
        dplyr::mutate(name = names) %>%
        tidyr::unnest(cols = c(data))
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
      # return early if there is no data
      if (is.null(data)) {
        return(NULL)
      }

      data %>%
        dplyr::pull(name) %>%
        unique() %>%
        sort()
    },

    # keep internal copies of the raw values before changing them using
    # transformations with given offsets and scale factors

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
      data <- data %>%
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
        # retaining them might confuse the user about whether the
        # transformations are supposed to be carried out by the user using these
        # values or these transformations have already been carried out
        # additionally, leave out internal copies of original data
        dplyr::select(
          -dplyr::matches("^paths$"),
          -dplyr::matches("offsets$|scalefactors$"),
          -dplyr::contains("original")
        ) %>%
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

#' Count number of objects
#'
#' @details
#'
#' If the argument is not a vector, unlike `length()`, this function will not
#' count the number of named bindings in an environment, but only the number of
#' instances of a class.
#'
#' For example, `length(mtcars)` will return 11, but `objCount(mtcars)` will
#' return 1.
#'
#' @param x An object (an atomic vector, a list, or instance(s) of a class).
#'
#' @examples
#'
#' objCount(c(1, 2, 3)) # 3
#' objCount(list("a", "b")) # 2
#' objCount(mtcars) # 1
#'
#' @return Integer representing the count of objects.
#'
#' @keywords internal
#' @noRd

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

#'  Validate arguments provided as vectors
#'
#' @details
#'
#' Validation of arguments provided as a vector involves:
#'
#' - Checking that it is of expected length.
#' - Checking for `NULL` or other unexpected values (`NaN`, `Inf`, `NA` of the
#'   wrong type) and standardizing them to `NA` of desired type.
#' - Checking that each element in the vector is of expected type.
#' - If a non-atomic list is provided, converting it to an atomic vector.
#'
#' @param x A vector of arguments.
#' @param expectedLength An integer to denote the expected length of the vector.
#' @inheritParams flattenList
#'
#' @return
#'
#' An atomic vector of desired type containing specified arguments.
#'
#' @examples
#'
#' validateVectorArgs(list(1, 2, NA, NULL), 4L, "numeric")
#' validateVectorArgs(c(1, 2, NA, NA_complex), 4L, "numeric")
#'
#' @keywords internal
#' @noRd

validateVectorArgs <- function(arg = NULL, expectedLength = NULL, type) {
  # return early if argument was not specified
  if (is.null(arg)) {
    return(NULL)
  }

  # validate the length of vector arguments
  if (!is.null(expectedLength)) {
    validateIsOfLength(arg, expectedLength)
  }

  # convert `NULL`s or logical `NA`s to `NA` of required type

  # Note that `purrr::map()` will return a list
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

#' Flatten a list to an atomic vector of desired type
#'
#' @param x A list or an atomic vector. If the latter, no change will be made.
#' @param type Type of atomic vector to be returned.
#'
#' @details
#'
#' The `type` argument will decide which variant from `purrr::flatten()` family
#' is used to flatten the list.
#'
#' @examples
#'
#' flattenList(list(1, 2, 3, NA), type = "numeric")
#' flattenList(list(TRUE, FALSE, NA), type = "integer")
#'
#' @return An atomic vector of desired type.
#'
#' @keywords internal
#' @noRd

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


#' Convert `NULL` or `NA`s to `NA` of desired type
#'
#' @param x A single element.
#' @inheritParams flattenList
#'
#' @examples
#'
#' toMissingOfType(NA, type = "real")
#' toMissingOfType(NULL, type = "integer")
#'
#' @keywords internal
#' @noRd

toMissingOfType <- function(x, type) {
  # all unexpected values will be converted to `NA` of a desired type
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
