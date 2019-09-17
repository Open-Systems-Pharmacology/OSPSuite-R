#' @title SimulationSettings
#' @docType class
#' @description  Settings associated with a given simulation
#'
#' @field solver Container containing all solver parameters for the simulation (read-only)
#' @field outputSelections All selected quantities (species, observers, parameters) that will be part of the simulated results
SimulationSettings <- R6::R6Class(
  "SimulationSettings",
  inherit = DotNetWrapper,
  active = list(
    solver = function(value) {
      if (missing(value)) {
        solver <- rClr::clrGet(self$ref, "Solver")
        SolverSettings$new(solver)
      } else {
        stop(messages$errorPropertyReadOnly("solver"), call. = FALSE)
      }
    },
    outputSelections = function(value) {
      if (missing(value)) {
        outputSelections <- rClr::clrGet(self$ref, "OutputSelections")
        OutputSelections$new(outputSelections)
      } else {
        stop(messages$errorPropertyReadOnly("outputSelections"), call. = FALSE)
      }
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  )
)
