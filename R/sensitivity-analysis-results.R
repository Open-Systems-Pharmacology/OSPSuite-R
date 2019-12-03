
#' @title SensitivityAnalysisResults
#' @docType class
#' @description  Results of a sensitivity analysis run (either individual or population simulation)
#' @section Methods:
#' \describe{
#'   \item{allPKParameterSensitivitiesFor(pkParameterName, outputPath, totalSensitivityThreshold)}
#'   {Returns the PKParameterSensitivity for a given pkParameter and output participating to a total sensitivity greater or equal to totalSensitivityThreshold}
#' }
#' @format NULL
SensitivityAnalysisResults <- R6::R6Class("SensitivityAnalysisResults",
  inherit = DotNetWrapper,
  public = list(
    allPKParameterSensitivitiesFor = function(pkParameterName, outputPath, totalSensitivityThreshold) {
      validateIsString(pkParameterName)
      validateIsString(outputPath)
      validateIsNumeric(totalSensitivityThreshold)
      pkParameterSentitivities <- rClr::clrCall(self$ref, "AllPKParameterSensitivitiesFor", pkParameterName, outputPath, totalSensitivityThreshold)
      toObjectType(pkParameterSentitivities, PKParameterSensitivity)
    },

    print = function(...) {
      private$printClass()
      invisible(self)
    }
  )
)
