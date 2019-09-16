
#' @title SimulationResults
#' @docType class
#' @description  Results of a simulation run (either individual or population simulation)
#'
#' @field count the number of individual results (\code{Count==1} generally means that we are dealing with an individual simulation results)
#' @format NULL
SimulationResults <- R6::R6Class("SimulationResults",
  inherit = DotNetWrapper,
  active = list(
    count = function(value) {
      private$wrapReadOnlyProperties("Count", value)
    }
  )
)
