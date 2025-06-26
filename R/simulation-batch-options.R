#' @title SimulationBatchOptions
#' @docType class
#' @description Options to be passed to the `SimulationBatch`.
#' @format NULL
SimulationBatchOptions <- R6::R6Class(
  "SimulationBatchOptions",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param variableParameters Vector of absolute parameter paths to be varied in a simulation batch
    #' @param variableMolecules Vector of absolute molecule paths to be varied in a simulation batch
    #' @return A new `SimulationBatchOptions` object.
    initialize = function(variableParameters = NULL, variableMolecules = NULL) {
      netObject <- rSharp::newObjectFromName("OSPSuite.R.Domain.SimulationBatchOptions")
      super$initialize(netObject)

      if (!is.null(variableMolecules)) {
        self$variableMolecules <- variableMolecules
      }
      if (!is.null(variableParameters)) {
        self$variableParameters <- variableParameters
      }
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "variableParameters" = self$variableParameters,
        "variableMolecules" = self$variableMolecules
      ))
    }
  ),
  active = list(
    #' @field variableParameters Vector of absolute parameter paths to be varied in a simulation batch
    variableParameters = function(value) {
      private$.wrapVectorProperty("VariableParameter", "VariableParameters", value, "Parameters")
    },
    #' @field variableMolecules Vector of absolute molecule paths to be varied in a simulation batch
    variableMolecules = function(value) {
      private$.wrapVectorProperty("VariableMolecule", "VariableMolecules", value, "Molecules")
    }
  )
)
