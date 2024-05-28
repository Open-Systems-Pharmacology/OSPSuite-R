#' @title SimulationBatchRunValues
#' @docType class
#' @description  Options to be passed to the SimulationBatch run
#' @export
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
      ref <- rClr::clrNew("OSPSuite.R.Domain.SimulationBatchRunValues")
      super$initialize(ref)
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
      private$printClass()
      private$printLine("parameterValues", self$parameterValues)
      private$printLine("initialValues", self$initialValues)
      invisible(self)
    }
  ),
  active = list(
    #' @field parameterValues Vector of parameter values used in a batch run
    parameterValues = function(value) {
      private$wrapVectorProperty("ParameterValue", "ParameterValues", value, "Values")
    },
    #' @field initialValues Vector of initial values used in a batch run
    initialValues = function(value) {
      private$wrapVectorProperty("InitialValue", "InitialValues", value, "MoleculeValues")
    },
    #' @field id Internal id of the batch run value
    id = function(value) {
      private$wrapReadOnlyProperty("Id", value)
    }
  )
)
