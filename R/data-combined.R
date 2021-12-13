#' @title Object combining simulated and observed data
#'
#' @description
#'
#' A class for storage of simulated and/or observed data, which can be further
#' used to extract as a dataframe or for visualization methods.
#'
#' @param simulationResults Instance of the `SimulationResults` object. A list
#'   of such instances will **not** be accepted.
#' @param dataSet Instance (or a list of instances) of the `DataSet` object(s).
#' @param groups TODO:
#' @param paths Quantity paths (element or vector of strings) for which the
#'   results are to be returned. When providing the paths, only absolute full
#'   paths are supported (i.e., no matching with '*' possible). If `NULL`
#'   (default value), returns the results for all output defined in the results.
#' @param individualIds A list of IDs of individuals whose simulations are of
#'   interest.
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
                                    paths = NULL,
                                    individualIds = NULL) {
      # list input is possible only for dataSet argument, and not here
      if (is.list(simulationResults)) {
        stop(
          "Only a single instance, and not a list, of `SimulationResults` objects is expected.",
          call. = FALSE
        )
      }

      # ascertain that the object is of the correct type
      validateIsOfType(simulationResults, "SimulationResults")

      # extract data
      private$.simulationResults <- simulationResults
    },

    #' @description
    #' Add observed data.
    #' @return `DataCombined` object containing observed data.

    addDataSet = function(dataSet, groups = NULL) {
      # if a list is provided, keep only elements which are of DataSet type
      if (is.list(dataSet)) {
        dataSet <- purrr::keep(dataSet, ~ inherits(.x, "DataSet"))

        # if no object of DataSet type is retained, inform the user
        if (length(dataSet) == 0L) {
          stop("No `DataSet` object detected.", call. = FALSE)
        }
      } else {
        validateIsOfType(dataSet, "DataSet")
      }

      private$.dataSet <- dataSet
    },

    ## getter methods ---------------

    #' @description
    #' A dataframe of simulated and observed data.
    #' @return A dataframe.

    toDataFrame = function() {
      # dataframe for observed data
      if (!is.null(private$.dataSet)) {
        if (is.list(private$.dataSet)) {
          dataObs <- purrr::map_dfr(private$.dataSet, dataSetToDataFrame)
        } else {
          dataObs <- dataSetToDataFrame(private$.dataSet)
        }
      }

      # dataframe for simulated data
      # if (!is.null(private$.simulationResults)) {
      #   dataSim <- dataSetToDataFrame(private$.simulationResults)
      # }

      # TODO: convert simulationResults to dataframe
      # if both not NULL, combine and return
      # if either is NULL, return the non-NULL one
      return(dataObs)
    },

    ## print method -----------------

    #' @description
    #' Print the object to the console
    #' If dataSet provided was a list of DataSet objects, this can be quite long

    print = function() {
      private$printClass()
      private$.simulationResults$print()

      if (is.list(private$.dataSet)) {
        purrr::walk(private$.dataSet, print)
      } else {
        private$.dataSet$print()
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
