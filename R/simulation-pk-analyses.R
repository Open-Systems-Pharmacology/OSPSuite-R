
#' @title SimulationPKAnalyses
#' @docType class
#' @description  pK-Analyses of a simulation (either individual or population simulation).
#'
SimulationPKAnalyses <- R6::R6Class(
  "SimulationPKAnalyses",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  private = list(
    .simulation = NULL,
    toPKParameter = function(netPKParameters) {
      toObjectType(netPKParameters, QuantityPKParameter)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param ref .NET reference
    #' @param simulation Simulation for which the pkParameters were calculated
    #' @return A new `SimulationPKAnalyses` object.
    initialize = function(ref, simulation) {
      validateIsOfType(simulation, Simulation)
      private$.simulation <- simulation
      super$initialize(ref)
    },
    #' @description
    #' Returns all QuantityPKParameter defined for a given path
    #' @param quantityPath Path for which pkParameters should be retrieved
    allPKParametersFor = function(quantityPath) {
      validateIsString(quantityPath)
      private$toPKParameter(rClr::clrCall(self$ref, "AllPKParametersFor", quantityPath))
    },
    #' @description
    #' The pK Parameter defined for the given path and name
    #' @param quantityPath Path for which the pkParameter named `pkParameter`
    #'   should be retrieved
    #' @param pkParameter Name of the pkParameter to retrieve
    pKParameterFor = function(quantityPath, pkParameter) {
      validateIsString(quantityPath)
      validateIsString(pkParameter)
      private$toPKParameter(rClr::clrCall(self$ref, "PKParameterFor", quantityPath, pkParameter))
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      private$printLine("For outputs", addTab = FALSE)
      for (quantityPath in self$allQuantityPaths) {
        private$printLine(quantityPath)
      }
      private$printLine("For pK-Parameters", addTab = FALSE)
      for (pkParameter in self$allPKParameterNames) {
        private$printLine(pkParameter)
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field simulation Reference to the `Simulation` used to calculate or import the PK-Analyses (Read-Only)
    simulation = function(value) {
      private$readOnlyProperty("simulation", value, private$.simulation)
    },
    #' @field allPKParameterNames Returns the name of all pk parameters for which a value is available
    allPKParameterNames = function(value) {
      private$readOnlyProperty("allPKParameterNames", value, rClr::clrGet(self$ref, "AllPKParameterNames"))
    },
    #' @field allQuantityPaths Returns the path of all quantities for which pk parameters were calculated
    allQuantityPaths = function(value) {
      private$readOnlyProperty("allQuantityPaths", value, rClr::clrGet(self$ref, "AllQuantityPaths"))
    }
  )
)
