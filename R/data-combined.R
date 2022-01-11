#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storage of simulated and/or observed data, which can be further
#' used to extract as a dataframe or for visualization methods.
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
#'   group. If an entry within the list is `NULL`, the corresponding data set
#'   is not assigned to any group. If `NULL` (default), all data sets are not
#'   assigned to any group. If provided, `groups` must have the same length as
#'   `dataSets`.
#' @param names A list of names specifying which observed datasets or paths in
#'   simulated dataset to transform.
#' @param xOffsets,yOffsets,xScaleFactors,yScaleFactors Either a numeric scalar
#'   or a list of numeric quantities specifying offsets and scale factors to
#'   apply to raw values. The default offset is `0`, while default scale factor
#'   is `1`, i.e., the data will not be modified. If a list is specified, it
#'   should be the same length as `names` argument.
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
                                    names = NULL, # TODO:
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

      # they should be of same length
      if (!is.null(groups)) {
        validateIsSameLength(simulationResults$allQuantityPaths, groups)

        # if a list is specified, convert NULL to NA
        # and then flatten list to a vector
        if (is.list(groups)) {
          groups <- purrr::flatten_chr(purrr::modify(groups, .null_to_na))
        }
      }

      # save the original object as it is; useful for `$toDataFrame()` method
      # styler: off
      private$.simulationResults              <- simulationResults
      private$.groups$groupsSimulationResults <- groups
      private$.quantitiesOrPaths              <- quantitiesOrPaths
      private$.population                     <- population
      private$.individualIds                  <- individualIds
      # styler: on

      # extract a dataframe and store it internally
      private$.simulationResultsDF <- private$.simResults2DF(private$.simulationResults, groups)

      # add it to combined dataframe
      private$.dataCombinedDF <- private$.updateDF(private$.dataCombinedDF, private$.simulationResultsDF)

      if (length(purrr::compact(private$.groups)) > 0L) {
        private$.groupMap <- private$.extractGroupMap(private$.dataCombinedDF)
      }
    },

    #' @description
    #' Add observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSets = function(dataSets, groups = NULL) {
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
      private$.dataSetsDF <- private$.dataSet2DF(private$.dataSets, groups)

      # add it to combined dataframe
      private$.dataCombinedDF <- private$.updateDF(private$.dataCombinedDF, private$.dataSetsDF)

      if (length(purrr::compact(private$.groups)) > 0L) {
        private$.groupMap <-  private$.extractGroupMap(private$.dataCombinedDF)
      }
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
    },

    ## getter methods ---------------

    #' @description
    #' A dataframe of simulated and/or observed data (depending on instances of
    #' which objects have been added to the object).
    #' @return A dataframe.

    toDataFrame = function() {
      # data extraction ----------------

      # if both not NULL, combine them
      # if either is NULL, use the non-NULL one
      if (!is.null(private$.dataSetsDF) && !is.null(private$.simulationResultsDF)) {
        private$.dataCombinedDF <- dplyr::bind_rows(private$.dataSetsDF, private$.simulationResultsDF)
      } else {
        private$.dataCombinedDF <- private$.dataSetsDF %||% private$.simulationResultsDF
      }

      # data transformations ----------------

      if (!is.null(private$.dataCombinedDF)) {
        # select only the selected dataset names and paths
        if (!is.null(private$.names)) {
          private$.dataCombinedDF <- dplyr::filter(private$.dataCombinedDF, name %in% private$.names)

          # if separate offsets and scale factors are provided for each name, then store
          # the respective values for each name, otherwise all rows gets the same value

          # offset for x-axis
          if (length(private$.xOffsets) > 1L) {
            validateIsSameLength(private$.xOffsets, private$.names)

            names(private$.xOffsets) <- private$.names
            private$.dataCombinedDF <- private$.dataCombinedDF %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(xOffsets = private$.xOffsets[match(name, private$.names)][[1]]) %>%
              dplyr::ungroup()
          } else {
            private$.dataCombinedDF <- dplyr::mutate(private$.dataCombinedDF, xOffsets = private$.xOffsets[[1]])
          }

          # offset for y-axis
          if (length(private$.yOffsets) > 1L) {
            validateIsSameLength(private$.yOffsets, private$.names)

            private$.dataCombinedDF <- private$.dataCombinedDF %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(yOffsets = private$.yOffsets[match(name, private$.names)][[1]]) %>%
              dplyr::ungroup()
          } else {
            private$.dataCombinedDF <- dplyr::mutate(private$.dataCombinedDF, yOffsets = private$.yOffsets[[1]])
          }

          # scale factor for x-axis
          if (length(private$.xScaleFactors) > 1L) {
            validateIsSameLength(private$.xScaleFactors, private$.names)

            private$.dataCombinedDF <- private$.dataCombinedDF %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(xScaleFactors = private$.xScaleFactors[match(name, private$.names)][[1]]) %>%
              dplyr::ungroup()
          } else {
            private$.dataCombinedDF <- dplyr::mutate(private$.dataCombinedDF, xScaleFactors = private$.xScaleFactors[[1]])
          }

          if (length(private$.yScaleFactors) > 1L) {
            validateIsSameLength(private$.yScaleFactors, private$.names)


            # scale factor for y-axis
            private$.dataCombinedDF <- private$.dataCombinedDF %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(yScaleFactors = private$.yScaleFactors[match(name, private$.names)][[1]]) %>%
              dplyr::ungroup()
          } else {
            private$.dataCombinedDF <- dplyr::mutate(private$.dataCombinedDF, yScaleFactors = private$.yScaleFactors[[1]])
          }
        } else {
          private$.dataCombinedDF <- dplyr::mutate(
            private$.dataCombinedDF,
            xOffsets = private$.xOffsets[[1]],
            yOffsets = private$.yOffsets[[1]],
            xScaleFactors = private$.xScaleFactors[[1]],
            yScaleFactors = private$.yScaleFactors[[1]]
          )
        }

        # apply transformations
        private$.dataCombinedDF <- dplyr::mutate(
          private$.dataCombinedDF,
          xValues = (xValues + xOffsets) * xScaleFactors,
          yValues = (yValues + yOffsets) * yScaleFactors
        )

        # applicable only if the error is available
        if ("yErrorValues" %in% names(private$.dataCombinedDF)) {
          private$.dataCombinedDF <- dplyr::mutate(
            private$.dataCombinedDF,
            yErrorValues = yErrorValues * yScaleFactors
          )
        }

        # these columns are no longer necessary
        # retaining them might confuse the user about whether the
        # transformations are supposed to be carried out by the user using these
        # values or these transformations have already been carried out by the
        # method using these values
        private$.dataCombinedDF <- dplyr::select(private$.dataCombinedDF, -dplyr::ends_with(c("Offsets", "ScaleFactors")))
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

    #' @field groups A named `list` specifying which data sets should be grouped
    #'   together.

    # just a way to access whatever was specified
    groups = function(value) {
      if (missing(value)) {
        private$.groups
      } else {
        stop(messages$errorPropertyReadOnly(
          "groupings",
          optionalMessage = "Data sets are assigned to groups when adding via `$addSimulationResults()` or `$addDataSets()` methods."
        ))
      }
    },

    #' @field groupMap A dataframe specifying which data sets have been grouped
    #'   together and the name of the dataset.

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

  # private fields and methods -----------------------------------

  private = list(
    # methods --------------------

    # extract dataframe from DataSet objects
    .dataSet2DF = function(object, groups = NULL) {
      # if a list of DataSet instances, merge iterated dataframes by rows
      if (is.list(object)) {
        data <- purrr::map_dfr(object, dataSetToDataFrame)
      } else {
        data <- dataSetToDataFrame(object)
      }

      # add column describing the type of data
      data <- data %>%
        dplyr::mutate(dataType = "observed", .before = 1L) %>%
        dplyr::as_tibble()

      if (!is.null(groups)) {
        data <- private$.addGroupCol(data, groups)
      }

      return(data)
    },

    # extract dataframe from SimulationResults objects
    .simResults2DF = function(object, groups = NULL) {
      # all input validation will take place in this function itself
      data <- simulationResultsToDataFrame(
        simulationResults = object,
        quantitiesOrPaths = private$.quantitiesOrPaths,
        population        = private$.population,
        individualIds     = private$.individualIds
      )

      # add column describing the type of data
      data <- dplyr::mutate(data, dataType = "simulated", .before = 1L) %>%
        dplyr::as_tibble()

      # rename according to column naming conventions for DataSet
      data <- dplyr::rename(data,
        "xValues"    = "Time",
        "xUnit"      = "TimeUnit",
        "yValues"    = "simulationValues",
        "yUnit"      = "unit",
        "yDimension" = "dimension"
      )

      # if names are not specified, use paths as unique names
      if (!"name" %in% names(data)) {
        data <- dplyr::mutate(data, name = paths)
      }

      if (!is.null(groups)) {
        data <- private$.addGroupCol(data, groups)
      }

      return(data)
    },

    # add a new group column
    .addGroupCol = function(data, groups) {
      data %>%
        dplyr::group_by(name) %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(group = groups, .before = 1L) %>%
        tidyr::unnest(cols = c(data)) %>%
        dplyr::mutate(
          group = dplyr::case_when(
            is.na(group) ~ name,
            TRUE ~ group
          )
        )
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

    # fields
    .dataSets = NULL,
    .dataSetsDF = NULL,
    .simulationResults = NULL,
    .simulationResultsDF = NULL,
    .dataCombinedDF = NULL,
    .groups = list(groupsDataSets = NULL, groupsSimulationResults = NULL),
    .groupMap = NULL,
    .quantitiesOrPaths = NULL,
    .population = NULL,
    .individualIds = NULL,
    .names = NULL,
    .xOffsets = 0,
    .yOffsets = 0,
    .xScaleFactors = 1,
    .yScaleFactors = 1
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
