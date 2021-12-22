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
#' @param dataSet Instance (or a list of instances) of the `DataSet` object(s).
#' @param groups TODO:
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
#' myCombDat$addDataSet(dataSet)
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
    #' @return A new instance of `DataCombined` object.

    # empty constructor because we prefer that the users add simulationResults
    # and DataSet objects separately and individually
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
      # list input is possible only for `dataSet` argument, and not here
      if (is.list(simulationResults)) {
        stop(
          "Only a single instance, and not a list, of `SimulationResults` objects is expected.",
          call. = FALSE
        )
      }

      # extract data
      # validation happens in the function itself
      private$.simulationResults <- simulationResultsToDataFrame(
        simulationResults = simulationResults,
        quantitiesOrPaths = quantitiesOrPaths,
        population = population,
        individualIds = individualIds
      )
    },

    #' @description
    #' Add observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSet = function(dataSet, groups = NULL) {
      # if a list is provided, keep only elements which are of `DataSet` type
      # if a single object, validation happens in `dataSetToDataFrame()` function
      if (is.list(dataSet)) {
        dataSet <- purrr::keep(dataSet, ~ inherits(.x, "DataSet"))

        # if no object of `DataSet` type is retained, inform the user
        if (length(dataSet) == 0L) {
          stop("No `DataSet` object detected.", call. = FALSE)
        }
      }

      if (is.list(dataSet)) {
        private$.dataSet <- purrr::map_dfr(dataSet, dataSetToDataFrame)
      } else {
        private$.dataSet <- dataSetToDataFrame(dataSet)
      }
    },

    ## getter methods ---------------

    #' @description
    #' A dataframe of simulated and/or observed data (depending on instances of
    #' which objects have been added to the object).
    #' @return A dataframe.

    toDataFrame = function() {
      # dataframe for observed data
      if (!is.null(private$.dataSet)) {
        # add column describing the type of data
        dataObs <- dplyr::mutate(private$.dataSet, dataType = "observed", .before = 1) %>%
          dplyr::as_tibble()
      }

      # dataframe for simulated data
      if (!is.null(private$.simulationResults)) {
        # add column describing the type of data
        dataSim <- dplyr::mutate(private$.simulationResults, dataType = "simulated", .before = 1) %>%
          dplyr::as_tibble()

        # rename according to column naming conventions for DataSet
        dataSim <- dplyr::rename(dataSim,
          "xValues" = "Time",
          "xUnit" = "TimeUnit",
          "yValues" = "simulationValues",
          "yUnit" = "unit",
          "yDimension" = "dimension"
        )
      }

      # if both not NULL, combine and return
      if (!is.null(private$.dataSet) && !is.null(private$.simulationResults)) {
        return(dplyr::bind_rows(dataObs, dataSim))
      }

      # if either is NULL, return the non-NULL one
      if (!is.null(private$.dataSet) && is.null(private$.simulationResults)) {
        return(dataObs)
      }
      if (is.null(private$.dataSet) && !is.null(private$.simulationResults)) {
        return(dataSim)
      }
    },

    ## print method -----------------

    #' @description
    #' Print the object to the console
    #' If dataSet provided was a list of DataSet objects, this can be quite long

    print = function() {
      private$printClass()

      if (!is.null(private$.simulationResults)) {
        private$.simulationResults$print()
      }

      if (!is.null(private$.dataSet)) {
        if (is.list(private$.dataSet)) {
          purrr::walk(private$.dataSet, print)
        } else {
          private$.dataSet$print()
        }
      }

      invisible(self)
    }
  ),

  # private fields and methods -----------------------------------

  private = list(
    .dataSet           = NULL,
    .simulationResults = NULL
  ),

  # other object properties --------------------------------------

  lock_objects = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  portable = TRUE
)
