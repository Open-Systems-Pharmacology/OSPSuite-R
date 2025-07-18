#' @title SimulationBatchRunValues
#' @docType class
#' @description  Options to be passed to the SimulationBatch run
#' @format NULL
SimulationBatchRunValues <- R6::R6Class(
  "SimulationBatchRunValues",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param parameterValues Vector of parameter values
    #' @param initialValues Vector of molecule initial values
    #' @return A new `SimulationBatchRunValues` object.
    initialize = function(parameterValues = NULL, initialValues = NULL) {
      netObject <- rSharp::newObjectFromName("OSPSuite.R.Domain.SimulationBatchRunValues")
      super$initialize(netObject)
      if (!is.null(parameterValues)) {
        self$parameterValues <- parameterValues
      }
      if (!is.null(initialValues)) {
        self$initialValues <- initialValues
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "parameterValues" = self$parameterValues,
        "initialValues" = self$initialValues
      ))
    }
  ),
  active = list(
    #' @field parameterValues Vector of parameter values used in a batch run
    parameterValues = function(value) {
      private$.wrapVectorProperty("ParameterValue", "ParameterValues", value, "Values")
    },
    #' @field initialValues Vector of initial values used in a batch run
    initialValues = function(value) {
      private$.wrapVectorProperty("InitialValue", "InitialValues", value, "MoleculeValues")
    },
    #' @field id Internal id of the batch run value
    id = function(value) {
      private$.wrapReadOnlyProperty("Id", value)
    }
  )
)
