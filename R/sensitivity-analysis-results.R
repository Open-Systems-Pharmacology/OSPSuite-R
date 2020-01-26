
#' @title SensitivityAnalysisResults
#' @docType class
#' @description  Results of a sensitivity analysis run (either individual or population simulation)
#'
#' @format NULL
SensitivityAnalysisResults <- R6::R6Class("SensitivityAnalysisResults",
  inherit = DotNetWrapper,
  public = list(
    #' @description
    #' Returns the PKParameterSensitivity for a given pkParameter and output participating to a total sensitivity
    #' greater or equal to totalSensitivityThreshold
    #' @param pkParameterName Name of pkParameter for which sensitivity should be retrieved
    #' @param outputPath Path of the output for which the sensitivity should be retrieved
    #' @param totalSensitivityThreshold Threshold used to filter out the most sensitive parameter. A threshold of 0.9 means that only
    #' parameter participating to a total of 90% of the sensitivity would be returned. A value of 1 would return the sensitivity for all parameters
    allPKParameterSensitivitiesFor = function(pkParameterName, outputPath, totalSensitivityThreshold) {
      validateIsString(pkParameterName)
      validateIsString(outputPath)
      validateIsNumeric(totalSensitivityThreshold)
      pkParameterSentitivities <- rClr::clrCall(self$ref, "AllPKParameterSensitivitiesFor", pkParameterName, outputPath, totalSensitivityThreshold)
      toObjectType(pkParameterSentitivities, PKParameterSensitivity)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      invisible(self)
    }
  )
)
