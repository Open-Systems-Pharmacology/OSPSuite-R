
#' @title SimulationPKAnalyses
#' @docType class
#' @description  PKAnalyses of a simulation (either individual or population simulation)
SimulationPKAnalyses <- R6Class("SimulationPKAnalyses",
  inherit = DotNetWrapper,
  public = list(
    allPKParametersFor = function(quantityPath) {
      toPKParameter(rClr::clrCall(self$ref, "AllPKParametersFor", quantityPath))
    }
  )
)
