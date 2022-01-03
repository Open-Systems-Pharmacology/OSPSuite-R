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
#' @param groups A string or a list of strings assigning the data set to a
#'   group. If an entry within the list is `NULL`, the corresponding data set is
#'   not assigned to any group. If `NULL` (default), all data sets are not
#'   assigned to any group. If provided, `groups` must have the same length as
#'   `dataSets`.
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
      # list of `SimulationResults` instances is not allowed
      if (is.list(simulationResults)) {
        stop(
          "Only a single instance, and not a list, of `SimulationResults` objects is expected.",
          call. = FALSE
        )
      }

      # save the original object
      ospsuite.utils::validateIsOfType(simulationResults, SimulationResults)
      private$.simulationResults <- simulationResults

      # extract a dataframe and store it internally
      # all input validation will take place in this function itself
      private$.simulationResultsDF <- simulationResultsToDataFrame(
        simulationResults = simulationResults,
        quantitiesOrPaths = quantitiesOrPaths,
        population        = population,
        individualIds     = individualIds
      )
    },

    #' @description
    #' Add observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSets = function(dataSets, groups = NULL) {
      # if a list is provided, keep only elements which are of `DataSet` type
      if (is.list(dataSets)) {
        dataSets <- purrr::keep(dataSets, ~ inherits(.x, "DataSet"))

        # if no object of `DataSet` type is retained, inform the user
        if (length(dataSets) == 0L) {
          stop("No `DataSet` object detected.", call. = FALSE)
        }
      } else {
        ospsuite.utils::validateIsOfType(dataSets, DataSet)
      }

      private$.dataSets <- dataSets

      if (is.list(dataSets)) {
        private$.dataSetsDF <- purrr::map_dfr(dataSets, dataSetToDataFrame)
      } else {
        private$.dataSetsDF <- dataSetToDataFrame(dataSets)
      }
    },

    ## getter methods ---------------

    #' @description
    #' A dataframe of simulated and/or observed data (depending on instances of
    #' which objects have been added to the object).
    #' @return A dataframe.

    toDataFrame = function() {
      # dataframe for observed data
      if (!is.null(private$.dataSetsDF)) {
        # add column describing the type of data
        private$.dataSetsDF <- dplyr::mutate(private$.dataSetsDF, dataType = "observed", .before = 1L) %>%
          dplyr::as_tibble()
      }

      # dataframe for simulated data
      if (!is.null(private$.simulationResultsDF)) {
        # add column describing the type of data
        private$.simulationResultsDF <- dplyr::mutate(private$.simulationResultsDF, dataType = "simulated", .before = 1L) %>%
          dplyr::as_tibble()

        # rename according to column naming conventions for DataSet
        private$.simulationResultsDF <- dplyr::rename(private$.simulationResultsDF,
          "xValues" = "Time",
          "xUnit" = "TimeUnit",
          "yValues" = "simulationValues",
          "yUnit" = "unit",
          "yDimension" = "dimension"
        )
      }

      # if both not NULL, combine and return
      # if either is NULL, return the non-NULL one
      if (!is.null(private$.dataSetsDF) && !is.null(private$.simulationResultsDF)) {
        return(dplyr::bind_rows(private$.dataSetsDF, private$.simulationResultsDF))
      } else {
        return(private$.dataSetsDF %||% private$.simulationResultsDF)
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
    .dataSets = NULL,
    .dataSetsDF = NULL,
    .simulationResults = NULL,
    .simulationResultsDF = NULL,
    .groups = NULL
  ),

  # other object properties --------------------------------------

  lock_objects = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  portable = TRUE
)
