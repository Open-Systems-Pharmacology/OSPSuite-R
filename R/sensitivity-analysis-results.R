#' @title SensitivityAnalysisResults
#' @docType class
#' @description Results of a sensitivity analysis run (either individual or
#'   population simulation).
#'
#' @format NULL
SensitivityAnalysisResults <- R6::R6Class("SensitivityAnalysisResults",
  inherit = DotNetWrapper,
  cloneable = FALSE,
  private = list(
    .simulation = NULL
  ),
  active = list(

    #' @field simulation Reference to the `Simulation` used to calculate or
    #'   import the sensitivity analysis results (Read-Only).
    simulation = function(value) {
      private$.readOnlyProperty("simulation", value, private$.simulation)
    },

    #' @field count the number of pk parameter sensitivity entries
    count = function(value) {
      private$.wrapReadOnlyProperty("Count", value)
    },

    #' @field allPKParameterNames Returns the name of all PK-Parameters
    #'   available in this results. This will be a subset of all potential
    #'   PK-Parameters available in the system.
    allPKParameterNames = function(value) {
      private$.wrapReadOnlyProperty("AllPKParameterNames", value)
    },

    #' @field allQuantityPaths Returns the path of all outputs available in this
    #'   results.
    allQuantityPaths = function(value) {
      private$.wrapReadOnlyProperty("AllQuantityPaths", value)
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param netObject A `NetObject`.
    #' @param simulation Reference to the simulation object used to calculated
    #'   the results.
    #' @return A new `SensitivityAnalysisResults` object.
    initialize = function(netObject, simulation) {
      validateIsOfType(simulation, "Simulation")
      private$.simulation <- simulation
      super$initialize(netObject)
    },

    #' @description
    #'
    #' Returns the `PKParameterSensitivity` for a given `pkParameter` and output
    #' participating to a total sensitivity greater or equal to
    #' `totalSensitivityThreshold`.
    #'
    #' @param pkParameterName Name of `pkParameter` for which sensitivity should
    #'   be retrieved.
    #' @param outputPath Path of the output for which the sensitivity should be
    #'   retrieved
    #' @param totalSensitivityThreshold Threshold used to filter out the most
    #'   sensitive parameter. A threshold of `0.9` means that only parameter
    #'   participating to a total of `90` percent of the sensitivity would be
    #'   returned. A value of `1` would return the sensitivity for all
    #'   parameters.
    allPKParameterSensitivitiesFor = function(pkParameterName,
                                              outputPath,
                                              totalSensitivityThreshold = ospsuiteEnv$sensitivityAnalysisConfig$totalSensitivityThreshold) {
      validateIsString(pkParameterName)
      validateIsString(outputPath)
      validateIsNumeric(totalSensitivityThreshold)

      pkParameterSentitivities <- self$call("AllPKParameterSensitivitiesFor", pkParameterName, outputPath, totalSensitivityThreshold)

      .toObjectType(pkParameterSentitivities, PKParameterSensitivity)
    },

    #' @description
    #'
    #' Returns the sensitivity value for a given `pkParameter`, output and model
    #' parameter (either by path or by name). If the sensitivity result does not
    #' exist, returns `NaN`.
    #'
    #' @param pkParameterName Name of `pkParameter` for which sensitivity should
    #'   be retrieved.
    #' @param outputPath Path of the output for which the sensitivity should be
    #'   retrieved.
    #' @param parameterName Name of the sensitivity parameter for which the
    #'   sensitivity should be retrieved.
    #' @param parameterPath Path of the sensitivity parameter for which the
    #'   sensitivity should be retrieved. Wildcards (*) not accepted.
    pkParameterSensitivityValueFor = function(pkParameterName,
                                              outputPath,
                                              parameterName = NULL,
                                              parameterPath = NULL) {
      validateIsCharacter(parameterName, nullAllowed = TRUE)
      validateIsCharacter(parameterPath, nullAllowed = TRUE)
      if ((!is.null(parameterName) && !is.null(parameterPath)) ||
        (is.null(parameterName) && is.null(parameterPath))) {
        stop(messages$errorOneOfNameAndPathMustBeSpecified())
      }

      if (!is.null(parameterName)) {
        value <- self$call(
          "PKParameterSensitivityValueBySensitivityParameterName",
          pkParameterName,
          outputPath,
          parameterName
        )
      }

      if (!is.null(parameterPath)) {
        value <- self$call(
          "PKParameterSensitivityValueByParameterPath",
          pkParameterName,
          outputPath,
          parameterPath
        )
      }

      value[is.nan(value)] <- NA_real_

      return(value)
    },

    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::osp_print_class(self)
      ospsuite.utils::osp_print_items(list(
        "Number of calculated sensitivities" = self$count
      ))
      ospsuite.utils::osp_print_items(self$allPKParameterNames, title = "Available PK Parameters")
      ospsuite.utils::osp_print_items(self$allQuantityPaths, title = "For the following outputs")
    }
  )
)
