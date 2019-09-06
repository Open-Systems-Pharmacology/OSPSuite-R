
#' @title SimulationPKAnalyses
#' @docType class
#' @description  PKAnalyses of a simulation (either individual or population simulation).
#' @section Methods:
#' \describe{
#'   \item{allPKParametersFor(quantityPath)}{All pK Parameters defined for a given path}
#'   \item{pKParameterFor(quantityPath, pkParameter)}{The pK Parameter defined for the given path and name}
#'   }
#'
SimulationPKAnalyses <- R6::R6Class("SimulationPKAnalyses",
  inherit = DotNetWrapper,
  public = list(
    allPKParametersFor = function(quantityPath) {
      validateIsString(quantityPath)
      toPKParameter(rClr::clrCall(self$ref, "AllPKParametersFor", quantityPath))
    },
    pKParameterFor = function(quantityPath, pkParameter) {
      validateIsString(quantityPath)
      validateIsString(pkParameter)
      toPKParameter(rClr::clrCall(self$ref, "PKParameterFor", quantityPath, pkParameter))
    }
  )
)
