#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storage of simulated and/or observed data, which can be further
#' used to extract as a dataframe or for visualization methods.
#'
#' @param simulationResults Object of type `SimulationResults` produced by
#'   calling `runSimulation` on a `Simulation` object.
#' @param quantitiesOrPaths Quantity instances (element or vector) typically
#'   retrieved using `getAllQuantitiesMatching` or quantity path (element or
#'   vector of strings) for which the results are to be returned. (optional)
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
#' @param groups A string or a vector of strings assigning the data set to a
#'   group. If an entry within the vector is `NULL`, the corresponding data set
#'   is not assigned to any group. If `NULL` (default), all data sets are not
#'   assigned to any group. If provided, `groups` must have the same length as
#'   `dataSets`.
#' @param names A vector of names specifying which observed datasets or paths in
#'   simulated dataset to transform.
#' @param xOffsets,yOffsets,xScaleFactors,yScaleFactors Either a numeric scalar
#'   or a vector of numeric quantities specifying offsets and scale factors to
#'   apply to raw values. The default offset is `0`, while default scale factor
#'   is `1`, i.e., the data will not be modified. If a vector is specified, it
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
                                    quantitiesOrPaths = NULL,
                                    population = NULL,
                                    individualIds = NULL) {
      # fail fast if this is not correct
      validateIsString(groups, nullAllowed = TRUE)

      # list of `SimulationResults` instances is not allowed
      if (is.list(simulationResults)) {
        stop(
          "Only a single instance, and not a list, of `SimulationResults` objects is expected.",
          call. = FALSE
        )
      }

      # validate the object type
      validateIsOfType(simulationResults, SimulationResults)

      # save the original object as it is; useful for `$toDataFrame()` method
      # styler: off
      private$.simulationResults <- simulationResults
      private$.groups            <- groups
      private$.quantitiesOrPaths <- quantitiesOrPaths
      private$.population        <- population
      private$.individualIds     <- individualIds
      # styler: on
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

      # save the original object as it is; useful for `$toDataFrame()` method
      private$.dataSets <- dataSets
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
      # dataframe for observed data
      if (!is.null(private$.dataSets)) {
        # if a list of DataSet instances, merge iterated dataframes by rows
        if (is.list(private$.dataSets)) {
          private$.dataSetsDF <- purrr::map_dfr(private$.dataSets, dataSetToDataFrame)
        } else {
          private$.dataSetsDF <- dataSetToDataFrame(private$.dataSets)
        }

        # add column describing the type of data
        private$.dataSetsDF <- dplyr::mutate(private$.dataSetsDF, dataType = "observed", .before = 1L) %>%
          dplyr::as_tibble()
      }

      # dataframe for simulated data
      if (!is.null(private$.simulationResults)) {
        # extract a dataframe and store it internally
        # all input validation will take place in this function itself
        private$.simulationResultsDF <- simulationResultsToDataFrame(
          simulationResults = private$.simulationResults,
          quantitiesOrPaths = private$.quantitiesOrPaths,
          population        = private$.population,
          individualIds     = private$.individualIds
        )

        # add column describing the type of data
        private$.simulationResultsDF <- dplyr::mutate(private$.simulationResultsDF, dataType = "simulated", .before = 1L) %>%
          dplyr::as_tibble()

        # rename according to column naming conventions for DataSet
        private$.simulationResultsDF <- dplyr::rename(private$.simulationResultsDF,
          "xValues"    = "Time",
          "xUnit"      = "TimeUnit",
          "yValues"    = "simulationValues",
          "yUnit"      = "unit",
          "yDimension" = "dimension"
        )

        # if names are not specified, use paths as unique names
        if (!"name" %in% names(private$.simulationResultsDF)) {
          private$.simulationResultsDF <- dplyr::mutate(private$.simulationResultsDF, name = paths)
        }
      }

      # if both not NULL, return the combined one
      # if either is NULL, return the non-NULL one
      if (!is.null(private$.dataSetsDF) && !is.null(private$.simulationResultsDF)) {
        private$.dataCombinedDF <- dplyr::bind_rows(private$.dataSetsDF, private$.simulationResultsDF)
      } else {
        private$.dataCombinedDF <- private$.dataSetsDF %||% private$.simulationResultsDF
      }

      # data transformations
      if (!is.null(private$.dataCombinedDF)) {
        # select only the selected dataset names and paths
        if (!is.null(private$.names)) {
          private$.dataCombinedDF <- dplyr::filter(private$.dataCombinedDF, name %in% private$.names)

          # if separate offsets and scale factors are provided for each name, then store
          # the respective values for each name, otherwise all rows gets the same value

          # offset for x-axis
          if (length(private$.xOffsets) > 1L) {
            if (length(private$.xOffsets) != length(private$.names)) {
              stop("Length of `xOffsets` argument should either be 1 or the same as `names` argument.", call. = FALSE)
            }

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
            if (length(private$.yOffsets) != length(private$.names)) {
              stop("Length of `yOffsets` argument should either be 1 or the same as `names` argument.", call. = FALSE)
            }

            private$.dataCombinedDF <- private$.dataCombinedDF %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(yOffsets = private$.yOffsets[match(name, private$.names)][[1]]) %>%
              dplyr::ungroup()
          } else {
            private$.dataCombinedDF <- dplyr::mutate(private$.dataCombinedDF, yOffsets = private$.yOffsets[[1]])
          }

          # scale factor for x-axis
          if (length(private$.xScaleFactors) > 1L) {
            if (length(private$.xScaleFactors) != length(private$.names)) {
              stop("Length of `xScaleFactors` argument should either be 1 or the same as `names` argument.", call. = FALSE)
            }

            private$.dataCombinedDF <- private$.dataCombinedDF %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(xScaleFactors = private$.xScaleFactors[match(name, private$.names)][[1]]) %>%
              dplyr::ungroup()
          } else {
            private$.dataCombinedDF <- dplyr::mutate(private$.dataCombinedDF, xScaleFactors = private$.xScaleFactors[[1]])
          }

          if (length(private$.yScaleFactors) > 1L) {
            if (length(private$.yScaleFactors) != length(private$.names)) {
              stop("Length of `yScaleFactors` argument should either be 1 or the same as `names` argument.", call. = FALSE)
            }

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
    }
  ),

  # private fields and methods -----------------------------------

  private = list(
    .dataSets            = NULL,
    .dataSetsDF          = NULL,
    .simulationResults   = NULL,
    .simulationResultsDF = NULL,
    .dataCombinedDF      = NULL,
    .groups              = NULL,
    .quantitiesOrPaths   = NULL,
    .population          = NULL,
    .individualIds       = NULL,
    .names               = NULL,
    .xOffsets            = 0,
    .yOffsets            = 0,
    .xScaleFactors       = 1,
    .yScaleFactors       = 1
  ),

  # other object properties --------------------------------------

  lock_objects = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  portable = TRUE
)
